{-# LANGUAGE OverloadedStrings #-}
-- | interp 的 hspec 测试套件。
--
-- 运行方式（不需要 cabal）：
--     ./test/run-spec.sh
-- 或者（需要 interp.cabal 的 test-suite 依赖 already 齐备）：
--     cabal test
--
-- 分组约定：
--   * 「正确行为」各分组 —— 今天应当全绿。红了就是回归。
--   * 「程序模式」       —— def / SCC / 会话状态（TODO 02 第 7 项），全绿。
--   * 「已知缺陷」       —— 断言的是*当前错误*的行为，今天绿。
--                          哪天它红了，说明缺陷被修好了，请把该条搬到上面去。
module Main where

import Interp.Types
import Interp.Parser (run, runProg)
import Interp.TypeCheck (typeChecker, programChecker)
import Interp.Eval (eval, evalProgram)
import Interp.Pretty

import Data.Text (Text)
import Data.Either (isLeft, isRight)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.IO as SIO
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Test.Hspec
import Test.QuickCheck

---------------------------------------------------------------------------
-- 把整条流水线（parse → typecheck → eval）压成一个可断言的值
--
-- 注意这里走的是「单表达式」入口 run/runProg 之外的老路径：run 只认表达式，
-- 所以这一层测不到 def —— def 的用例都在下面「程序模式」那组。
---------------------------------------------------------------------------

data Outcome
    = ParseErr Text
    | TypeErr  Text
    | EvalErr  Text
    | Ok Text Text          -- ^ 类型, 值
    deriving (Show, Eq)

runSrc :: Text -> Outcome
runSrc src = case run src of
    Left perr -> ParseErr perr
    Right ast -> case tcRun ast of
        Left terr -> TypeErr (tcMsg terr)
        Right ty  -> case evRun ast of
            Left eerr -> EvalErr (evMsg eerr)
            Right v   -> Ok (prettyT' ty) (prettyV' v)

tcRun :: E' -> Either (Located TypeError) T'
tcRun ast = fmap snd (fst (runState (runExceptT (typeChecker M.empty ast)) 0))

evRun :: E' -> Either (Located EvalError) V'
evRun ast = runReaderT (eval ast) M.empty

-- | 丢掉位置，只留消息本身 —— 断言文案时用这个。
tcMsg :: Located TypeError -> Text
tcMsg (Located _ e) = prettyTypeError e

evMsg :: Located EvalError -> Text
evMsg (Located _ e) = prettyEvalError e

---------------------------------------------------------------------------
-- 程序模式：整段源码走 parseProg → programChecker → evalProgram
--
-- 这是 TODO 02 第 7 项（REPL 会话状态 + 文件级定义）的库层接口，
-- 也是 app/Main.hs 的 execBatch 去掉 IO 之后的形状。
---------------------------------------------------------------------------

data ProgRes = ProgRes
    { prTEnv :: TEnv
    , prEnv  :: Env
    , prNext :: Counter
    , prTys  :: M.Map Int StmtTy
    , prVals :: M.Map Int (Either (Located EvalError) V')
    } deriving (Show)

emptyProg :: ProgRes
emptyProg = ProgRes M.empty M.empty 0 M.empty M.empty

-- | 跑一段源码，返回新的会话。任一步失败就 Left（带上渲染好的消息）。
--
--   语句下标是 0 起的**序号**，不是源码行号 —— 和 programChecker / evalProgram
--   的索引一致。app/Main.hs 里 printBatch 要打 N: 前缀时得先换算成行号。
progRun :: ProgRes -> Text -> Either Text ProgRes
progRun s src = case runProg src of
    Left perr -> Left ("Parse error: " <> perr)
    Right lprs ->
        let prs = map snd lprs
            (tcRes, n') = runState (runExceptT (programChecker (prTEnv s) prs)) (prNext s)
        in case tcRes of
            Left terr -> Left ("Type error: " <> prettyTypeErrorWith src terr)
            Right (_, tenv', tys) ->
                let (evRes, _) = runState
                        (runReaderT (runExceptT (evalProgram prs)) (prEnv s)) (Depth 0)
                in case evRes of
                    Left eerr -> Left ("Eval error: " <> prettyEvalErrorWith src eerr)
                    Right (env', vals) -> Right ProgRes
                        { prTEnv = M.union tenv' (prTEnv s)   -- 新定义遮蔽旧定义
                        , prEnv  = env'
                        , prNext = n'
                        , prTys  = tys
                        , prVals = vals
                        }

-- | 多行会话：一行一次 progRun（模拟 REPL 逐行提交），
--   返回每一行跑完之后的会话；ss !! 0 是第一行之后的。任一行出错就 Left。
--
--   注意：**每行是一个独立的 Program**，所以语句下标每行都从 0 重来。
--   互递归的 def 必须写在同一次 progRun 里（同一份文件 = 同一个批），
--   逐行喂给它们只会得到 unbound variable。
progSteps :: [Text] -> Either Text [ProgRes]
progSteps = go emptyProg
  where
    go _ []     = Right []
    go s (x:xs) = case progRun s x of
        Left e   -> Left e
        Right s' -> (s' :) <$> go s' xs

-- | 取第 i 行跑完之后的会话；越界时给一个空会话（断言会以"<无值>"的形式报出来）。
at :: Int -> [ProgRes] -> ProgRes
at i ss = case drop i ss of
    (s:_) -> s
    []    -> emptyProg

-- | 期待全程不出错；出错就直接把消息当作断言失败打出来。
okSteps :: [Text] -> IO [ProgRes]
okSteps srcs = case progSteps srcs of
    Right ss -> return ss
    Left e   -> do
        expectationFailure ("不该出错：" <> T.unpack e)
        return []

-- | 单次 progRun（一份多行源码 = 一个批），出错就当作断言失败。
okProg :: Text -> IO ProgRes
okProg src = case progRun emptyProg src of
    Right s -> return s
    Left e  -> do
        expectationFailure ("不该出错：" <> T.unpack e)
        return emptyProg

-- | 第 i 条语句的类型文本：def 打泛化后的 scheme，表达式打实例化的 T'。
stmtType :: Int -> ProgRes -> Text
stmtType i s = case M.lookup i (prTys s) of
    Just (TyDef _ sch)      -> prettyS' sch
    Just (TyExpr (Right t)) -> prettyT' t
    Just (TyExpr (Left e))  -> "Type error: " <> tcMsg e
    Nothing                 -> "<无类型>"

-- | 第 i 条语句的值文本；def 没有值，表达式出错就打出错消息。
stmtVal :: Int -> ProgRes -> Text
stmtVal i s = case M.lookup i (prVals s) of
    Just (Right v) -> prettyV' v
    Just (Left e)  -> "Eval error: " <> evMsg e
    Nothing        -> "<无值>"

---------------------------------------------------------------------------
-- 断言助手
---------------------------------------------------------------------------

-- | 期待 Parse error，且消息里含某段文字。
parseErr :: Text -> Text -> Expectation
parseErr needle src = case runSrc src of
    ParseErr e -> e `shouldSatisfy` T.isInfixOf needle
    other      -> expectationFailure $ "期待 Parse error，实际是: " <> show other

typeErr :: Text -> Expectation
typeErr src = case runSrc src of
    TypeErr _ -> return ()
    other     -> expectationFailure $ "期待 Type error，实际是: " <> show other

evalErr :: Text -> Text -> Expectation
evalErr needle src = case runSrc src of
    EvalErr e -> e `shouldSatisfy` T.isInfixOf needle
    other     -> expectationFailure $ "期待 Eval error，实际是: " <> show other

-- | 断言「是个 Type error，且消息里含某段文字」。
hasTypeErrText :: Text -> Outcome -> Bool
hasTypeErrText needle o = case o of
    TypeErr t -> needle `T.isInfixOf` t
    _         -> False

-- | 错误渲染里应当出现插入符定位。
rendersCaret :: Text -> Text -> Expectation
rendersCaret needle src = case run src of
    Left _ -> expectationFailure "不该是 Parse error"
    Right ast -> case tcRun ast of
        Left terr -> do
            let txt = prettyTypeErrorWith src terr
            txt `shouldSatisfy` T.isInfixOf "^"
            txt `shouldSatisfy` T.isInfixOf needle
        Right _ -> expectationFailure "期待 Type error"

---------------------------------------------------------------------------

main :: IO ()
main = do
    -- 让中文断言消息在 Windows 控制台也能正常输出
    SIO.hSetEncoding SIO.stdout SIO.utf8
    hspec spec

spec :: Spec
spec = do

  -------------------------------------------------------------------------
  describe "词法与空白" $ do

    it "整数独占一行可以解析" $
        runSrc "123" `shouldBe` Ok "Int" "123"

    it "整数后面紧跟字母是 Parse error（不是把它当函数应用）" $
        parseErr "unexpected" "123abc"

    it "保留字不能当变量名" $
        parseErr "reserved" "let in = 1 in 2"

    it "单表达式入口 run 不认 def —— 它只在程序模式里合法" $
        parseErr "reserved" "def x = 1"

    it "标识符可以带数字、下划线和撇号" $
        runSrc "lambda x1' -> x1'" `shouldBe` Ok "(a0 -> a0)" "<closure x1' :: ...>"

    it "不带行首空白的普通表达式" $
        runSrc "1 + 2" `shouldBe` Ok "Int" "3"

  -------------------------------------------------------------------------
  describe "优先级与结合性" $ do

    it "乘号比加号紧"      $ runSrc "1 + 2 * 3" `shouldBe` Ok "Int" "7"
    it "幂号比乘号紧"      $ runSrc "2 * 3 ^ 2" `shouldBe` Ok "Int" "18"
    it "加号左结合"        $ runSrc "1 - 2 - 3" `shouldBe` Ok "Int" "-4"
    it "幂号右结合"        $ runSrc "2 ^ 3 ^ 2" `shouldBe` Ok "Int" "512"
    it "整除是向下取整"    $ runSrc "10 / 3"    `shouldBe` Ok "Int" "3"

  -------------------------------------------------------------------------
  describe "一元负号" $ do

    it "裸负数"                $ runSrc "-3"         `shouldBe` Ok "Int" "-3"
    it "连续负号"              $ runSrc "- -3"       `shouldBe` Ok "Int" "3"
    it "负号比幂号松"          $ runSrc "-2 ^ 2"     `shouldBe` Ok "Int" "-4"
    it "括号改变结合"          $ runSrc "(-2) ^ 2"   `shouldBe` Ok "Int" "4"
    it "减一个负数"            $ runSrc "2 - -3"     `shouldBe` Ok "Int" "5"

  -------------------------------------------------------------------------
  describe "整数是 Integer，不是机器字" $ do

    it "2^100 精确" $
        runSrc "2 ^ 100" `shouldBe`
            Ok "Int" "1267650600228229401496703205376"

    it "2^63 不溢出" $
        runSrc "2 ^ 63" `shouldBe` Ok "Int" "9223372036854775808"

    it "超长字面量" $
        runSrc "99999999999999999999999" `shouldBe`
            Ok "Int" "99999999999999999999999"

    it "除零是 Eval error" $
        evalErr "Divided by zero" "1 / 0"

  -------------------------------------------------------------------------
  describe "比较运算符" $ do

    it "1 < 2"   $ runSrc "1 < 2"   `shouldBe` Ok "Bool" "True"
    it "1 <= 2"  $ runSrc "1 <= 2"  `shouldBe` Ok "Bool" "True"
    it "2 < 2"   $ runSrc "2 < 2"   `shouldBe` Ok "Bool" "False"
    it "2 <= 2"  $ runSrc "2 <= 2"  `shouldBe` Ok "Bool" "True"
    it "1 > 2"   $ runSrc "1 > 2"   `shouldBe` Ok "Bool" "False"
    it "2 > 1"   $ runSrc "2 > 1"   `shouldBe` Ok "Bool" "True"
    it "1 >= 2"  $ runSrc "1 >= 2"  `shouldBe` Ok "Bool" "False"
    it "2 >= 2"  $ runSrc "2 >= 2"  `shouldBe` Ok "Bool" "True"
    it "1 == 2"  $ runSrc "1 == 2"  `shouldBe` Ok "Bool" "False"
    it "1 != 2"  $ runSrc "1 != 2"  `shouldBe` Ok "Bool" "True"
    it "比较的结果是 Bool，可以参与运算（经 if）" $
        runSrc "if 1 < 2 then 10 else 20" `shouldBe` Ok "Int" "10"

    -- 比较运算符的优先级低于 +-，所以右边整体是一个算术表达式。
    -- （这两条以前是「已知缺陷」，现已修正。）
    it "比较比 +- 松：1 < 2 + 3 解析成 1 < (2 + 3)" $
        runSrc "1 < 2 + 3" `shouldBe` Ok "Bool" "True"

    it "比较比 +- 松：0 == 1 - 1 解析成 0 == (1 - 1)" $
        runSrc "0 == 1 - 1" `shouldBe` Ok "Bool" "True"

    it "但 a + b < c 本来就对，所以旧缺陷很难察觉" $
        runSrc "1 + 1 == 2" `shouldBe` Ok "Bool" "True"

    it "== 只支持 Int，Bool 之间不能比较" $
        typeErr "true == false"

    it "比较结果不能和 Bool 比较" $
        typeErr "(1 < 2) == true"

  -------------------------------------------------------------------------
  describe "Bool 与 if" $ do

    it "true"  $ runSrc "true"  `shouldBe` Ok "Bool" "True"
    it "false" $ runSrc "false" `shouldBe` Ok "Bool" "False"

    it "if 取 then 分支" $
        runSrc "if true then 1 else 2" `shouldBe` Ok "Int" "1"

    it "if 取 else 分支" $
        runSrc "if false then 1 else 2" `shouldBe` Ok "Int" "2"

    it "条件不是 Bool 要报错" $
        typeErr "if 1 then 2 else 3"

    it "两个分支类型不一致要报错" $
        typeErr "if true then 1 else false"

    it "两个分支都是函数时，类型统一" $
        runSrc "if true then lambda x -> x else lambda y -> y"
            `shouldSatisfy` \o -> case o of
                Ok t _ -> t == "(a1 -> a1)"
                _      -> False

  -------------------------------------------------------------------------
  describe "lambda / 闭包 / 词法作用域" $ do

    it "单参数应用" $
        runSrc "(lambda x -> x * 2) 3" `shouldBe` Ok "Int" "6"

    it "柯里化两参数" $
        runSrc "(lambda x -> lambda y -> x + y - x * y) 3 4"
            `shouldBe` Ok "Int" "-5"

    it "裸 lambda 的类型是 a -> a" $
        runSrc "lambda x -> x" `shouldBe` Ok "(a0 -> a0)" "<closure x :: ...>"

    it "多参数缩写 lambda x y -> e 等价于柯里化" $
        runSrc "(lambda x y -> x - y) 7 2" `shouldBe` Ok "Int" "5"

    it "非函数被应用要报错" $
        typeErr "1 2"

    it "参数给多了要报错" $
        typeErr "(lambda x -> x) 1 2"

    it "自应用触发 occurs check" $
        typeErr "lambda x -> x x"

  -------------------------------------------------------------------------
  -- 这一组曾经是红的（Eval.hs 的 Let 无条件结绳），现已全绿。
  describe "let 求值" $ do

    it "let 绑非函数，取出来应当是那个值" $
        runSrc "let x = 3 in x" `shouldBe` Ok "Int" "3"

    it "let 绑非函数，参与算术" $
        runSrc "let x = 3 in x + 1" `shouldBe` Ok "Int" "4"

    it "嵌套 let" $
        runSrc "let a = 1 in let b = 2 in a + b * 3" `shouldBe` Ok "Int" "7"

    it "let 绑函数，应用它应当真的归约一层" $
        runSrc "let id = lambda x -> x in id 5" `shouldBe` Ok "Int" "5"

    it "let 绑柯里化函数" $
        runSrc "let k = lambda x -> lambda y -> x in k 1 2" `shouldBe` Ok "Int" "1"

    it "let 绑非函数，括号包裹" $
        runSrc "let x = 1 in(x)" `shouldBe` Ok "Int" "1"

    it "lambda 体内嵌 let" $
        runSrc "(lambda x -> let y = 5 in lambda z -> x - y + 2 * z) 2 7"
            `shouldBe` Ok "Int" "11"

    it "内层 let 遮蔽外层，且内层 RHS 看见的是外层 x" $
        runSrc "let x = 1 in let x = x + 1 in x" `shouldBe` Ok "Int" "2"

    it "两个并列的 let" $
        runSrc "let x = 3 in let y = 4 in x * y" `shouldBe` Ok "Int" "12"

    it "let 绑定的值只求值一次（共享）" $
        runSrc "let big = 12345 * 6789 in big + big"
            `shouldBe` Ok "Int" "167620410"

    it "递归函数：fact 5" $
        runSrc "let fact = lambda n -> if n == 0 then 1 else n * fact (n - 1) in fact 5"
            `shouldBe` Ok "Int" "120"

    it "递归函数：fact 20（不溢出）" $
        runSrc "let fact = lambda n -> if n == 0 then 1 else n * fact (n - 1) in fact 20"
            `shouldBe` Ok "Int" "2432902008176640000"

    it "let 绑定的函数是多态的（对 Bool 也能用）" $
        runSrc "let id = lambda x -> x in id true" `shouldBe` Ok "Bool" "True"

    it "多态 let 在同一次求值里用两种类型" $
        runSrc "let id = lambda x -> x in if id true then id 5 else 2"
            `shouldBe` Ok "Int" "5"

    it "词法作用域：闭包捕获定义处的 a，不是调用处的" $
        runSrc "let a = 1 in let f = lambda u -> a in let a = 2 in f 0"
            `shouldBe` Ok "Int" "1"

  -------------------------------------------------------------------------
  describe "程序模式：def / SCC / 会话状态" $ do

    it "runProg 会跳过行首空白和空行（run 不会）" $ do
        ss <- okSteps ["\n\n   1 + 2\n"]
        stmtVal 0 (at 0 ss) `shouldBe` "3"
        runSrc "   1 + 2" `shouldSatisfy` \o -> case o of
            ParseErr _ -> True
            _          -> False

    it "0 参 def：def x = 1 先定义，下一行的表达式取到它" $ do
        ss <- okSteps ["def x = 1", "x"]
        stmtType 0 (at 0 ss) `shouldBe` "Int"
        stmtVal  0 (at 1 ss) `shouldBe` "1"

    it "带参 def 的类型是泛化后的 scheme，不是实例化的 T'" $ do
        ss <- okSteps ["def id x = x"]
        -- a1 而不是 a0：sccChecker 先给 def 自己 fresh 一个（a0），
        -- 再给 lambda 的参数 fresh（a1），泛化时 a0 被消掉。
        stmtType 0 (at 0 ss) `shouldBe` "Forall a1. (a1 -> a1)"

    it "带参 def 能应用" $ do
        ss <- okSteps ["def f x = x + 1", "f 10"]
        stmtVal 0 (at 1 ss) `shouldBe` "11"

    it "多参数 def 等价于柯里化" $ do
        ss <- okSteps ["def add x y = x + y", "add 3 4"]
        stmtVal 0 (at 1 ss) `shouldBe` "7"

    -- ---------------------------------------------------------------------
    -- def 体里调用另一个 def
    --
    -- 这一组是 2026-09-23 补的：在此之前整套用例里**没有任何一条**让 def 体
    -- 去引用另一个 def，于是 Eval.hs 里「闭包只捕获本 SCC 的 knot」这个缺陷
    -- 从套件里溜了过去（用户报的 max2 is an unbound variable 就是它）。
    -- 下面这些在修好之前全红。
    -- ---------------------------------------------------------------------

    it "def 体能调用【同一个批】里更早定义的 def（两者不同 SCC）" $ do
        -- f 和 g 之间没有互相依赖，所以是两个独立的 AcyclicSCC。
        -- step 是拓扑序跑的，跑到 g 时 env 里已经有 f 了 —— 但 g 的闭包
        -- 捕获的必须是这个 env，而不是只含 g 自己的 knot。
        s <- okProg (T.unlines
            [ "def f x = x + 1"
            , "def g x = f x"
            , "g 1" ])
        stmtVal 2 s `shouldBe` "2"

    it "def 体能调用【上一行】定义的 def（跨批，会话 Env 必须真的接上）" $ do
        ss <- okSteps ["def f x = x + 1", "def g x = f x", "g 1"]
        stmtVal 0 (at 2 ss) `shouldBe` "2"

    it "跨批三级链 f → g → h" $ do
        ss <- okSteps ["def f x = x + 1", "def g x = f x", "def h x = g x", "h 1"]
        stmtVal 0 (at 3 ss) `shouldBe` "2"

    it "def 体能读到会话里 0 参 def 的值" $ do
        ss <- okSteps ["def k = 5", "def g x = k + x", "g 1"]
        stmtVal 0 (at 2 ss) `shouldBe` "6"

    it "参数名遮蔽同名的外层 def：def b a = a + 1 里的 a 是参数" $ do
        ss <- okSteps ["def a x = x", "def b a = a + 1", "b 1"]
        stmtVal 0 (at 2 ss) `shouldBe` "2"

    it "回归：用户报的 max2/max3 —— 两个 def 分两行，第三行调用" $ do
        ss <- okSteps
            [ "def max2 a b = if a < b then b else a"
            , "def max3 a b c = if max2 a b == a then if max2 a c == a then a else c else if max2 b c == b then b else c"
            , "max3 1 4 3" ]
        stmtType 0 (at 2 ss) `shouldBe` "Int"
        stmtVal  0 (at 2 ss) `shouldBe` "4"

    it "互递归：even/odd 在同一个批里由同一个 SCC 一起定型并一起结绳" $ do
        -- 互递归的两个 def 必须同属一个 Program：逐行喂（REPL 那样）时
        -- odd 还没定义，even 的闭包自然看不到它 —— 这是会话语义，不是缺陷。
        -- （同一个批里它们互相可见，靠的仍是 knot 本身。）
        s <- okProg (T.unlines
            [ "def even n = if n == 0 then true else odd (n - 1)"
            , "def odd  n = if n == 0 then false else even (n - 1)"
            , "even 4" ])
        stmtType 2 s `shouldBe` "Bool"
        stmtVal  2 s `shouldBe` "True"

    it "会话里重定义遮蔽旧定义（TEnv 与 Env 都要覆盖）" $ do
        ss <- okSteps ["def x = 1", "def x = 2", "x"]
        stmtVal 0 (at 2 ss) `shouldBe` "2"
        -- 类型环境里也只剩一份 x，且是新的那次
        stmtType 0 (at 1 ss) `shouldBe` "Int"

    it "计数器跨行单调：第二行的类型变量编号不回到 0" $ do
        ss1 <- okSteps ["lambda a -> a"]
        ss2 <- okSteps ["lambda a -> a", "lambda b -> b"]
        stmtType 0 (at 1 ss2) `shouldBe` "(a1 -> a1)"
        prNext (at 1 ss2) `shouldSatisfy` (> prNext (at 0 ss1))

    it "一行里只要有类型错误，整行不提交（调用方保留旧会话）" $ do
        ss <- okSteps ["def x = 1"]
        let s1 = at 0 ss
        progRun s1 "def x = 1 + true" `shouldSatisfy` isLeft
        -- 旧的会话没被污染：x 仍然是 1
        case progRun s1 "x" of
            Right s3 -> stmtVal 0 s3 `shouldBe` "1"
            Left e   -> expectationFailure ("s1 应当还能用：" <> T.unpack e)

    it "def 绑定的语法是 =，lambda 参数用 ->（两者不一致，见报告）" $ do
        progRun emptyProg "def f x = x + 1"  `shouldSatisfy` isRight
        progRun emptyProg "def f x -> x + 1" `shouldSatisfy` isLeft

  -------------------------------------------------------------------------
  describe "错误位置渲染（Span）" $ do

    it "未绑定变量：有插入符，且源行回显正确" $
        rendersCaret "zzz is an unbound variable." "zzz"

    it "插入符的列号 == 出错 token 在源码行里的列号" $ do
        let src = "1 + zzz"
            txt = case run src of
                    Right ast -> case tcRun ast of
                                    Left terr -> prettyTypeErrorWith src terr
                                    Right _   -> ""
                    Left _ -> ""
            ls = T.lines txt
        length ls `shouldBe` 3
        -- ls!!0 = "1 | 1 + zzz"（源码行回显）, ls!!1 = "  |     ^"
        T.findIndex (== '^') (ls !! 1) `shouldBe` T.findIndex (== 'z') (ls !! 0)

  -------------------------------------------------------------------------
  -- 下面这些断言的是【当前错误的行为】，所以今天是绿的。
  -- 哪天变红了 = 缺陷被修好，请把对应条目搬到上面「正确行为」去。
  describe "已知缺陷（断言当前行为，修好后请搬走）" $ do

    it "let x = x in 1：类型检查放行，求值才报错（Let 的 RHS 检查时把 x 放进了环境）" $
        runSrc "let x = x in 1" `shouldBe` EvalErr "x is an unbound variable."

    it "互递归只有 def 支持；let 仍然不行（let 不是互递归结点）" $
        runSrc "let even = lambda n -> if n == 0 then true else odd (n - 1) in even 4"
            `shouldSatisfy` \o -> case o of
                TypeErr t -> T.isInfixOf "odd is an unbound variable" t
                _         -> False

  -------------------------------------------------------------------------
  describe "属性测试（QuickCheck）" $ do

    it "加法全链路成立" $
        property $ forAll (choose (-1000, 1000) :: Gen Integer) $ \a ->
        forAll (choose (-1000, 1000) :: Gen Integer) $ \b ->
            runSrc (T.pack (show a) <> " + " <> T.pack (show b))
                === Ok "Int" (T.pack (show (a + b)))

    it "乘法全链路成立" $
        property $ forAll (choose (-1000, 1000) :: Gen Integer) $ \a ->
        forAll (choose (-1000, 1000) :: Gen Integer) $ \b ->
            runSrc (T.pack (show a) <> " * " <> T.pack (show b))
                === Ok "Int" (T.pack (show (a * b)))

    -- 注意实参要加括号：`f -3` 会被解析成减法 `f - 3`（Haskell 也是这样）
    it "恒等函数应用" $
        property $ forAll (choose (-10000, 10000) :: Gen Integer) $ \a ->
            runSrc ("(lambda x -> x) (" <> T.pack (show a) <> ")")
                === Ok "Int" (T.pack (show a))

    it "f -3 被解析成减法，不是「把 f 应用到 -3」" $
        typeErr "(lambda x -> x) -3"

    it "小于号与 Prelude 一致" $
        property $ forAll (choose (-1000, 1000) :: Gen Integer) $ \a ->
        forAll (choose (-1000, 1000) :: Gen Integer) $ \b ->
            runSrc (T.pack (show a) <> " < " <> T.pack (show b))
                === Ok "Bool" (T.pack (show (a < b)))

    it "括号包裹不改变语义" $
        property $ forAll (choose (-1000, 1000) :: Gen Integer) $ \a ->
            runSrc (T.pack (show a)) === runSrc ("(" <> T.pack (show a) <> ")")
