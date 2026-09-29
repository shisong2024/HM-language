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
import Interp.TypeCheck (typeChecker, programChecker, buildDEnv)
import Interp.Eval (evalwDepth, evalProgram, dataDeclsOf)
import Interp.Builtin (initSession, emptyDEnv)
import Interp.Pretty

import Data.Text (Text)
import Data.Either (isLeft, isRight)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.IO as SIO
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState, evalState)
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

-- 会话起点一律取 initSession：内置函数（cons）在它里面。
-- 少了这一层，`cons 1 []` 会以 "cons is an unbound variable" 失败 ——
-- 那是测试环境比真实环境（app/Main.hs 也是从 initSession 起）少东西，
-- 不是被测代码的问题。
-- 单表达式走不到 data 声明，所以 DEnv 一律取空的那个 —— 和 initSession 的 sDEnv 一致。
tcRun :: E' -> Either (Located TypeError) T'
tcRun ast = fmap snd (fst (runState (runExceptT (runReaderT (typeChecker (sTEnv initSession) ast) (sDEnv initSession))) (sNext initSession)))

-- 走 evalwDepth 而不是裸 eval：加深度守卫时 eval 的签名会多出一个
-- MonadState Depth，裸 eval 那行就编不过了 —— 而 evalwDepth 两种签名下都在。
evRun :: E' -> Either (Located EvalError) V'
evRun ast = evalState (runExceptT (runReaderT (evalwDepth ast) (sEnv initSession))) (Depth 0)

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
    , prDEnv :: DEnv      -- ^ data 声明累积出来的构造子/类型环境，跨批保留
    , prTys  :: M.Map Int StmtTy
    , prVals :: M.Map Int (Either (Located EvalError) V')
    }

-- | ⚠️ 手写 Show，**不能** derive。
--
--   `Env = Map Text V'`、`V' = VClosure Text E' Env`，而 Eval 给递归 def 打结
--   （闭包捕获的环境里就有它自己）：于是 `show` 任何一个含 def 的 Env 都会
--   无限展开 —— 不是打印得难看，是**挂死并吃光内存**。
--
--   这不是假想的：hspec 的 `shouldSatisfy` / `shouldBe` 失败时会把实际值
--   `show` 进失败消息，所以只要有一条断言在这一层失败，整个 spec.exe 就
--   变成「卡住 + OOM」（2026-09-28 实测，见 TODO 14）。环境字段一律只打大小。
instance Show ProgRes where
    show s = "ProgRes { prTEnv = <" <> show (M.size (prTEnv s)) <> " defs>"
        <> ", prEnv = <" <> show (M.size (prEnv s)) <> " closures>"
        <> ", prNext = " <> show (prNext s)
        <> ", prDEnv = <" <> show (M.size (denvCtors (prDEnv s))) <> " ctors>"
        <> ", prTys = " <> show (prTys s)
        <> ", prVals = <" <> show (M.size (prVals s)) <> " values> }"

-- | 空会话 = 只有内置函数的起点，和 app/Main.hs 用 initSession 起的会话一致。
emptyProg :: ProgRes
emptyProg = ProgRes (sTEnv initSession) (sEnv initSession) (sNext initSession) (sDEnv initSession) M.empty M.empty

-- | 两批 data 声明合并 —— app/Main.hs 里叫 unionDEnv，这边只需要同样的语义：
--   新声明赢。buildDEnv 也在 Main 里被调过一次，但 programChecker 内部还会
--   自己从本批的 dataDeclsOf 再建一份，这里补的是"上一批"那一半。
unionDEnv :: DEnv -> DEnv -> DEnv
unionDEnv a b = DEnv (M.union (denvCtors a) (denvCtors b)) (M.union (denvDatas a) (denvDatas b))

-- | 跑一段源码，返回新的会话。任一步失败就 Left（带上渲染好的消息）。
--
--   语句下标是 0 起的**序号**，不是源码行号 —— 和 programChecker / evalProgram
--   的索引一致。app/Main.hs 里 printBatch 要打 N: 前缀时得先换算成行号。
progRun :: ProgRes -> Text -> Either Text ProgRes
progRun s src = case runProg src of
    Left perr -> Left ("Parse error: " <> perr)
    Right lprs ->
        let prs = map snd lprs
            -- 本批的 data 声明 ＋ 之前各批留下的（REPL 逐行提交时靠这个把
            -- data Maybe 传给下一行的构造子模式），和 app/Main.hs 的 execBatch 同构。
            denv = unionDEnv (buildDEnv (dataDeclsOf prs)) (prDEnv s)
            (tcRes, n') = runState (runExceptT (runReaderT (programChecker (prTEnv s) prs) denv)) (prNext s)
        in case tcRes of
            -- ⚠️ 注意实参顺序：renderLocated 的实现是 (batchName, src)，
            -- 而 prettyTypeErrorWith 的签名却写成 (src, batchName) —— 两者都是 Text，
            -- 编译器分不出来。这里必须按【实现】的顺序传：batchName 在前。
            -- Spec 里的 span 名是 runProg 用的 ""，所以插入符照常渲染。
            Left terr -> Left ("Type error: " <> prettyTypeErrorWith "" src terr)
            Right (_, tenv', tys) -> case defFailure src tys of
                -- 一个 def 没通过（或因为依赖失败而没检查），这一批就不算成功。
                -- 和 app/Main.hs 的 execBatch 同构：只报错，不求值。
                Just msg -> Left ("Type error: " <> msg)
                Nothing ->
                    let (evRes, _) = runState
                            (runReaderT (runExceptT (evalProgram prs)) (prEnv s)) (Depth 0)
                    in case evRes of
                        Left eerr -> Left ("Eval error: " <> prettyEvalErrorWith "" src eerr)
                        Right (env', vals) -> Right ProgRes
                            { prTEnv = M.union tenv' (prTEnv s)   -- 新定义遮蔽旧定义
                            , prEnv  = env'
                            , prNext = n'
                            , prDEnv = denv
                            , prTys  = tys
                            , prVals = vals
                            }

-- | per-SCC 恢复（2026-09-28）之后，一批里有一个坏 def 不再让 programChecker
--   整体 Left —— 它返回 Right，把每个 def 的结果分别放进 prTys，于是一批里的
--   **所有** def 错误都能报出来，而不是只报第一个（见 app/Main.hs 的 printBatch）。
--
--   但「这一批算不算成功」不能跟着放宽：类型没定下来的程序求值没有意义，
--   而且闭包环境是自指的，`show` 一个求值结果会挂死。所以这里把 def 级失败
--   重新收成 Left。取**第一个**坏 def（M.elems 按语句下标升序，即最早那个）；
--   正常情况下 TyDefFailed 必然存在（有 def 被 skip 就说明它的依赖失败了）。
--
--   ⚠️ 因此本文件**依赖** test/rev/stage2b-scc.patch 引进的三个构造子
--      （TyDefFailed / TyDefSkipped / CalleeNotChecked）。补丁还没进 src/ 时
--      `test/run-spec.sh` 会**编译失败**（不是用例红，是压根编不过）——
--      那是缺补丁，不是 src 有类型错误。先打补丁，或跑
--      `SRC=test/rev/s2c/src bash test/run-spec.sh` 用影子副本验。
defFailure :: Text -> M.Map Int StmtTy -> Maybe Text
defFailure src tys = case [e | TyDefFailed e <- M.elems tys] of
    (e: _)  -> Just (prettyTypeErrorWith "" src e)
    []      -> case [ns | TyDefSkipped ns <- M.elems tys, not (null ns)] of
        (ns: _) -> Just ("not checked: " <> T.intercalate ", " ns)
        []      -> Nothing

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

-- | parseErr 的整段源码版：runSrc 走的是单表达式入口 run，认不出 def / 多行，
--   所以带 def 的解析错误要用这个（走的才是 REPL 与文件路径用的 runProg）。
progParseErr :: Text -> Text -> Expectation
progParseErr needle src = case runProg src of
    Left e   -> e `shouldSatisfy` T.isInfixOf needle
    Right _  -> expectationFailure "期待 Parse error，实际解析通过了"

-- | 同上的反面：报错里**不许**出现某段文字（专治「误报成 reserved symbol」这类回归）。
progParseErrNot :: Text -> Text -> Expectation
progParseErrNot banned src = case runProg src of
    Left e   -> e `shouldNotSatisfy` T.isInfixOf banned
    Right _  -> expectationFailure "期待 Parse error，实际解析通过了"

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
            let txt = prettyTypeErrorWith "" src terr
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

    it "1 < 2"   $ runSrc "1 < 2"   `shouldBe` Ok "Bool" "true"
    it "1 <= 2"  $ runSrc "1 <= 2"  `shouldBe` Ok "Bool" "true"
    it "2 < 2"   $ runSrc "2 < 2"   `shouldBe` Ok "Bool" "false"
    it "2 <= 2"  $ runSrc "2 <= 2"  `shouldBe` Ok "Bool" "true"
    it "1 > 2"   $ runSrc "1 > 2"   `shouldBe` Ok "Bool" "false"
    it "2 > 1"   $ runSrc "2 > 1"   `shouldBe` Ok "Bool" "true"
    it "1 >= 2"  $ runSrc "1 >= 2"  `shouldBe` Ok "Bool" "false"
    it "2 >= 2"  $ runSrc "2 >= 2"  `shouldBe` Ok "Bool" "true"
    it "1 == 2"  $ runSrc "1 == 2"  `shouldBe` Ok "Bool" "false"
    it "1 != 2"  $ runSrc "1 != 2"  `shouldBe` Ok "Bool" "true"
    it "比较的结果是 Bool，可以参与运算（经 if）" $
        runSrc "if 1 < 2 then 10 else 20" `shouldBe` Ok "Int" "10"

    -- 比较运算符的优先级低于 +-，所以右边整体是一个算术表达式。
    -- （这两条以前是「已知缺陷」，现已修正。）
    it "比较比 +- 松：1 < 2 + 3 解析成 1 < (2 + 3)" $
        runSrc "1 < 2 + 3" `shouldBe` Ok "Bool" "true"

    it "比较比 +- 松：0 == 1 - 1 解析成 0 == (1 - 1)" $
        runSrc "0 == 1 - 1" `shouldBe` Ok "Bool" "true"

    it "但 a + b < c 本来就对，所以旧缺陷很难察觉" $
        runSrc "1 + 1 == 2" `shouldBe` Ok "Bool" "true"

    -- 2026-09-25：== / != 从「两侧无条件钉死 TInt」改成结构化比较，
    -- 下面两条于是从「已知缺陷」（断言类型错）翻面成正向断言。
    it "== 是多态的：Bool 之间也能比" $
        runSrc "true == false" `shouldBe` Ok "Bool" "false"

    it "== 是多态的：比较结果也能再比" $
        runSrc "(1 < 2) == true" `shouldBe` Ok "Bool" "true"

  -------------------------------------------------------------------------
  describe "Bool 与 if" $ do

    it "true"  $ runSrc "true"  `shouldBe` Ok "Bool" "true"
    it "false" $ runSrc "false" `shouldBe` Ok "Bool" "false"

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
                Ok t _ -> t == "(a0 -> a0)"   -- 显示层按首次出现重编号
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
        runSrc "let id = lambda x -> x in id true" `shouldBe` Ok "Bool" "true"

    it "多态 let 在同一次求值里用两种类型" $
        runSrc "let id = lambda x -> x in if id true then id 5 else 2"
            `shouldBe` Ok "Int" "5"

    it "词法作用域：闭包捕获定义处的 a，不是调用处的" $
        runSrc "let a = 1 in let f = lambda u -> a in let a = 2 in f 0"
            `shouldBe` Ok "Int" "1"

    -- 以前这是「已知缺陷」：类型侧对任意 RHS 都开递归（先把 t 放进环境再查 e1），
    -- 于是 `x` 在 `e1 = Var x` 里解析成自己、unify (tv, tv) 通过，类型检查放行；
    -- 求值侧只在 e1 是 lambda 时才结绳，于是求值才报未绑定。
    -- 现在两侧都对齐成「只有 lambda 才递归」，所以这条是正向断言。
    it "let 的非 lambda RHS 看不见自己：x = x 是未绑定，不是递归" $
        runSrc "let x = x in 1" `shouldBe` TypeErr "x is an unbound variable."

    -- 上面那条修好的前提是「非 lambda 时用外层 env」，而不是「一律不进环境」：
    -- 内层的 e1 必须看得见外层的 x。下面这条（同组开头也有一条等价的）就是它的哨兵。
    it "非 lambda RHS 仍然看得见外层的同名变量" $
        runSrc "let x = 1 in let y = x + 1 in y" `shouldBe` Ok "Int" "2"

  -------------------------------------------------------------------------
  -- 解构 let：绑定位置收不可反驳模式（变量 / _ / 元组，含嵌套）。
  -- 实现是纯解析器脱糖：
  --     let (a, b) = e in body
  --   ==> Let "$let@L:C" e (Match (Var "$let@L:C") [(PTuple [PVar a, PVar b], body)])
  -- 单变量仍走原来的 Let，所以「let 绑定的递归函数」那条特例不受影响。
  -- 可反驳的模式（Just x / [] / 1 / h: t）直接是 Parse error —— let 没有失败分支。
  describe "let 解构绑定" $ do

    it "二元组" $
        runSrc "let (a, b) = (1, 2) in a + b" `shouldBe` Ok "Int" "3"

    it "三元组" $
        runSrc "let (a, b, c) = (1, 2, 3) in a * b + c" `shouldBe` Ok "Int" "5"

    it "嵌套元组" $
        runSrc "let (a, (b, c)) = (1, (2, 3)) in a + b + c" `shouldBe` Ok "Int" "6"

    it "元组里的 _" $
        runSrc "let (a, _) = (1, 2) in a" `shouldBe` Ok "Int" "1"

    it "整个模式就是 _" $
        runSrc "let _ = 5 in 7" `shouldBe` Ok "Int" "7"

    -- 脱糖成 Match 之后，PTuple 通过合一反过来把 e 的类型钉住 —— 这条是它的哨兵。
    it "被绑定的分量类型由 e 决定（不是自由变量）" $
        runSrc "let (a, b) = (1, true) in b" `shouldBe` Ok "Bool" "true"

    -- 回归哨兵：单变量路径必须原样保留 TypeCheck 里 recEnv 的自递归特例。
    it "单变量 let 的自递归没被影响" $
        runSrc "let f = lambda n -> if n == 0 then 1 else n * f (n - 1) in f 5"
            `shouldBe` Ok "Int" "120"

    it "脱糖后的体会正常报类型错" $
        runSrc "let (a, b) = (1, 2) in c"
            `shouldBe` TypeErr "c is an unbound variable."

    it "e 不是元组 → 类型错" $
        runSrc "let (a, b) = 1 in a" `shouldSatisfy` hasTypeErrText "Type mismatch"

    -- 临时名以 $ 开头（用户打不出来）。它绝不该出现在报错里。
    it "脱糖临时名（$let@…）不泄漏进报错" $
        case runSrc "let (a, b) = 1 in a" of
            TypeErr t -> t `shouldNotSatisfy` T.isInfixOf "$let"
            other     -> expectationFailure $ "期待 Type error，实际是: " <> show other

    -- 可反驳模式：解析期就拒，错误文案指向模式起点（列 5，正是 ( 的位置）。
    it "Just x → Parse error" $
        parseErr "cannot be used as a `let` binding" "let Just x = 1 in x"

    it "[] → Parse error" $
        parseErr "cannot be used as a `let` binding" "let [] = 1 in 1"

    it "整数字面量 → Parse error" $
        parseErr "cannot be used as a `let` binding" "let 1 = 2 in 1"

    it "h: t → Parse error" $
        parseErr "cannot be used as a `let` binding" "let h: t = [1] in h"

  -------------------------------------------------------------------------
  describe "列表与元组" $ do

    it "列表字面量：类型与渲染" $
        runSrc "[1,2,3]" `shouldBe` Ok "[Int]" "[1, 2, 3]"

    it "空列表的类型是多态 [a]，不是 [Int]" $
        runSrc "[]" `shouldSatisfy` \o -> case o of
            Ok t v -> T.isPrefixOf "[a" t && T.isSuffixOf "]" t
                   && not ("Int" `T.isInfixOf` t) && v == "[]"
            _      -> False

    it "嵌套列表" $
        runSrc "[[1],[2,3]]" `shouldBe` Ok "[[Int]]" "[[1], [2, 3]]"

    it "列表元素类型必须一致：异构列表是类型错" $
        runSrc "[1,true]" `shouldSatisfy` hasTypeErrText "Type mismatch"

    it "元组字面量：类型与渲染" $
        runSrc "(1,true)" `shouldBe` Ok "(Int, Bool)" "(1, true)"

    it "嵌套元组" $
        runSrc "(1,(2,true))" `shouldBe` Ok "(Int, (Int, Bool))" "(1, (2, true))"

    it "元组的两个位置各自独立多态" $
        runSrc "(lambda x -> x) (1,true)" `shouldBe` Ok "(Int, Bool)" "(1, true)"

    it "元组长度必须一致（if 两侧）" $
        runSrc "if true then (1,2) else (1,2,3)"
            `shouldSatisfy` hasTypeErrText "Type mismatch"

    it "列表里的元组" $
        runSrc "[(1,true),(2,false)]"
            `shouldBe` Ok "[(Int, Bool)]" "[(1, true), (2, false)]"

    -- 这是 B4 那类「两侧规则不一致」在元组/列表上的同型复查：
    -- 非 lambda 的 let RHS 看不见自己，所以是未绑定，不是 occurs check。
    it "let 的非 lambda RHS 里自引用：未绑定（列表/元组不会改变这个结论）" $
        runSrc "let p = (1,p) in 1" `shouldBe` TypeErr "p is an unbound variable."

    it "occurs check 仍然生效（列表里放自己）" $
        runSrc "lambda x -> [x, x]" `shouldSatisfy` \o -> case o of
            Ok t _ -> T.isPrefixOf "(a" t      -- (aN -> [aN])，不崩即可
            _      -> False

    -- 2026-09-25 翻面（原 B12 / TODO/08）：BOpr 不再把两侧钉死成 TInt，
    -- == / != 变成结构化比较 —— 列表、元组、Bool、用户 data 都能比。
    it "列表上的 == 是结构化比较：相同为 true" $
        runSrc "[1,2] == [1,2]" `shouldBe` Ok "Bool" "true"

    it "列表上的 == 是结构化比较：元素不同为 false" $
        runSrc "[1,2] == [1,3]" `shouldBe` Ok "Bool" "false"

    it "元组上的 == 是结构化比较：相同为 true" $
        runSrc "(1,2) == (1,2)" `shouldBe` Ok "Bool" "true"

    it "元组上的 != 是结构化比较：不同为 true" $
        runSrc "(1,2) != (1,3)" `shouldBe` Ok "Bool" "true"

  -------------------------------------------------------------------------
  -- 模式匹配（TODO/09-模式匹配.md）
  --
  -- 覆盖点按「哪一层负责」分组：
  --   * 求值    —— matchP 的形状匹配、臂的短路顺序、match 是表达式
  --   * cons    —— 中缀 `:` 脱糖 + VPrim 的归约。曾经的缺陷：实参补齐后
  --                判的是旧 args，于是 cons 一次都没归约，冒号构造的值
  --                是 VPrim，任何 cons 模式都匹配不上（2026-09-24 修好）。
  --   * 类型    —— 各臂结果类型统一、穷尽性的静态判定与「判不了就放行」
  --   * 查重    —— DuplicatePatVar。曾经的缺陷：checkDupPatVar 拿
  --                `S.toList . patVars` 去查重，Set 已经去过重，永远查不出
  --                （2026-09-24 修好，改走保序的 patVarsList）。
  -------------------------------------------------------------------------
  describe "模式匹配" $ do

    it "整数模式：从头往下试，第一个匹配的臂胜出" $
        runSrc "match 0 with 0 -> 1 | _ -> 2" `shouldBe` Ok "Int" "1"

    it "整数模式：前面的臂不匹配时落到后面的臂" $
        runSrc "match 1 with 0 -> 1 | _ -> 2" `shouldBe` Ok "Int" "2"

    it "Bool 模式" $
        runSrc "match true with false -> 1 | true -> 2" `shouldBe` Ok "Int" "2"

    it "列表模式：cons 臂绑定头与尾" $
        runSrc "match [1,2] with (x: xs) -> x | [] -> 0" `shouldBe` Ok "Int" "1"

    it "列表模式：空表臂" $
        runSrc "match [] with (x: xs) -> x | [] -> 0" `shouldBe` Ok "Int" "0"

    it "cons 模式不写括号也行（: 右结合，臂体不会把 | 吃掉）" $
        runSrc "match [1,2] with [] -> 0 | x : xs -> x" `shouldBe` Ok "Int" "1"

    it "元组模式：嵌套也能取到内层" $
        runSrc "match (1,(2,3)) with (x,(y,z)) -> y | _ -> 0" `shouldBe` Ok "Int" "2"

    it "通配符不绑定任何名字" $
        runSrc "match (1,2) with (x,_) -> x | _ -> 0" `shouldBe` Ok "Int" "1"

    it "臂体里可以用 if" $
        runSrc "match 1 with 0 -> 1 | _ -> if true then 2 else 3" `shouldBe` Ok "Int" "2"

    it "match 是表达式，能当实参" $
        runSrc "(lambda x -> x) (match 1 with _ -> 42)" `shouldBe` Ok "Int" "42"

    -- ---------------------------------------------------------------------
    -- cons / 中缀冒号
    -- ---------------------------------------------------------------------

    it "cons 是内置函数，实参够了就归约（不是留着不动的 VPrim）" $
        runSrc "cons 1 []" `shouldBe` Ok "[Int]" "[1]"

    it "中缀冒号脱糖成 cons" $
        runSrc "1 : 2 : []" `shouldBe` Ok "[Int]" "[1, 2]"

    it "cons 构造的值与列表字面量是同一种值（能被 cons 模式解构）" $
        runSrc "match (1 : [2, 3]) with (x: xs) -> x | [] -> 0" `shouldBe` Ok "Int" "1"

    it "cons 的元素类型跟着走（多态）" $
        runSrc "cons true []" `shouldBe` Ok "[Bool]" "[true]"

    -- ---------------------------------------------------------------------
    -- 类型侧
    -- ---------------------------------------------------------------------

    it "各臂的结果类型必须一致" $
        runSrc "match 1 with 0 -> 1 | _ -> true"
            `shouldSatisfy` hasTypeErrText "Type mismatch"

    it "元组模式的元数必须和 scrutinee 对得上" $
        runSrc "match (1,2) with (x,y,z) -> x | _ -> 0"
            `shouldSatisfy` hasTypeErrText "Type mismatch"

    it "列表模式不穷尽（只有 cons 臂）：类型检查就拒绝，不必等到运行期" $
        runSrc "match [1] with (x: xs) -> x"
            `shouldSatisfy` hasTypeErrText "not exhaustive"

    it "Bool 模式缺一半：类型检查拒绝" $
        runSrc "match true with true -> 1"
            `shouldSatisfy` hasTypeErrText "not exhaustive"

    -- 穷尽性判定是 best-effort：Int 上的字面量模式判不了，只能放行到运行期。
    it "Int 的穷尽性判不了 —— 放行，运行期报 Eval Error" $
        evalErr "not exhaustive" "match 5 with 0 -> 1"

    it "全通配臂能兜住任何类型" $
        runSrc "match (1,2) with _ -> 0" `shouldBe` Ok "Int" "0"

    -- ---------------------------------------------------------------------
    -- 重复绑定（DuplicatePatVar）
    -- ---------------------------------------------------------------------

    it "同一个模式里绑定两次：类型错" $
        runSrc "match (1, 2) with (x, x) -> x | _ -> 0"
            `shouldSatisfy` hasTypeErrText "bound more than once"

    it "嵌套模式里的重复也要抓到" $
        runSrc "match (1, (2, 3)) with (x, (x, z)) -> z | _ -> 0"
            `shouldSatisfy` hasTypeErrText "bound more than once"

    it "cons 模式里的重复也要抓到" $
        runSrc "match [1] with (x : x) -> x | _ -> 0"
            `shouldSatisfy` hasTypeErrText "bound more than once"

    it "报的是最先重复的那个名字" $
        runSrc "match (1, 2, 3) with (x, y, y) -> x | _ -> 0"
            `shouldSatisfy` hasTypeErrText "The variable y is"

    -- 下面两条是反向保护：查重必须【逐臂】做，不能跨臂、不能算上通配符。
    it "不同臂用同一个名字是合法的" $
        runSrc "match (1, 2) with (x, y) -> x | (x, y) -> y" `shouldBe` Ok "Int" "1"

    it "模式变量可以和外层同名（形参不算重复，臂内它是遮蔽的那个）" $ do
        ss <- okSteps ["def f x = match (x, 1) with (x, y) -> x + y | _ -> 0;", "f 2;"]
        stmtVal 0 (at 1 ss) `shouldBe` "3"

    -- ---------------------------------------------------------------------
    -- def + 模式匹配
    -- ---------------------------------------------------------------------

    it "def 里的 match：参数与结果都保持多态" $ do
        ss <- okSteps
            [ "def null l = match l with [] -> true | (x: xs) -> false;"
            , "null [1,2,3];"
            , "null [];" ]
        stmtType 0 (at 0 ss) `shouldBe` "Forall a0. ([a0] -> Bool)"
        stmtVal  0 (at 1 ss) `shouldBe` "false"
        stmtVal  0 (at 2 ss) `shouldBe` "true"

    -- 模式绑定的变量（y、xs）必须从 freeVars 里减掉，否则会污染 deps/SCC。
    it "模式绑定的变量不污染泛化：tail 能用在两种元素类型上" $ do
        ss <- okSteps
            [ "def tl2 l = match l with [] -> [] | (y: xs) -> xs;"
            , "tl2 [1,2];"
            , "tl2 [true];" ]
        stmtType 0 (at 0 ss) `shouldBe` "Forall a0. ([a0] -> [a0])"
        stmtType 0 (at 1 ss) `shouldBe` "[Int]"
        stmtType 0 (at 2 ss) `shouldBe` "[Bool]"

    it "递归 + 模式：自己写的 length（作用在 cons 构造的值上）" $ do
        ss <- okSteps
            [ "def len l = match l with [] -> 0 | (x: xs) -> len xs + 1;"
            , "len (cons 1 (cons 2 (cons 3 [])));" ]
        stmtVal 0 (at 1 ss) `shouldBe` "3"

    -- ---------------------------------------------------------------------
    -- 解析错误的措辞
    --
    -- parseMatch 曾经是 `withSpan $ try $ ...`：match 内部一出错就整体回溯，
    -- 关键字接着被当成普通表达式重新解析，于是报出 `reserved symbol: "match"`
    -- 并且 caret 指在 match 自己身上 —— 离真正缺的 token 十万八千里
    -- （2026-09-24 修好：见到关键字就提交，不再整体 try）。
    -- 下面三条钉住「报在正确的位置、并且提到正确的 token」。
    -- ---------------------------------------------------------------------

    it "少写 with：报错要提到 with，不是 reserved symbol" $
        progParseErr "\"with\"" "def f l = match l wit [] -> true"

    it "臂里少写 ->：报错要提到 ->" $
        progParseErr "->" "def f l = match l with [] true"

    it "臂体为空：缺表达式的地方要报 unexpected" $
        progParseErr "unexpected" "def null l = match l with [] -> "

    -- 曾经这是「已知缺陷」：parseIf / parseLet 外面包着 `withSpan $ try $ ...`，
    -- 少个 else / in 时报的是 `reserved symbol: "if"` 并指在关键字上，离真正
    -- 缺的 token 十万八千里。parseMatch 已先修好，2026-09-27 这两条也修好了。
    it "少写 else / in：报 unexpected，不再误报 reserved symbol" $ do
        progParseErr    "unexpected"      "def f = if true then 1 else"
        progParseErrNot "reserved symbol" "def f = if true then 1 else"
        progParseErr    "unexpected"      "def f = let x = 1"
        progParseErrNot "reserved symbol" "def f = let x = 1"

  -------------------------------------------------------------------------
  describe "程序模式：def / SCC / 会话状态" $ do

    it "runProg 会跳过行首空白和空行（run 不会）" $ do
        ss <- okSteps ["\n\n   1 + 2;\n"]
        stmtVal 0 (at 0 ss) `shouldBe` "3"
        runSrc "   1 + 2" `shouldSatisfy` \o -> case o of
            ParseErr _ -> True
            _          -> False

    it "0 参 def：def x = 1 先定义，下一行的表达式取到它" $ do
        ss <- okSteps ["def x = 1;", "x;"]
        stmtType 0 (at 0 ss) `shouldBe` "Int"
        stmtVal  0 (at 1 ss) `shouldBe` "1"

    it "带参 def 的类型是泛化后的 scheme，不是实例化的 T'" $ do
        ss <- okSteps ["def id x = x;"]
        -- 编号由显示层（prettyS'''）按首次出现重排，所以永远是 a0；
        -- 计数器本身仍是会话全局单调的 —— 真正的单调性断言在下面的
        -- 「计数器跨行单调」里（那一条断的是 prNext，与显示无关）。
        stmtType 0 (at 0 ss) `shouldBe` "Forall a0. (a0 -> a0)"

    it "带参 def 能应用" $ do
        ss <- okSteps ["def f x = x + 1;", "f 10;"]
        stmtVal 0 (at 1 ss) `shouldBe` "11"

    it "多参数 def 等价于柯里化" $ do
        ss <- okSteps ["def add x y = x + y;", "add 3 4;"]
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
            [ "def f x = x + 1;"
            , "def g x = f x;"
            , "g 1;" ])
        stmtVal 2 s `shouldBe` "2"

    it "def 体能调用【上一行】定义的 def（跨批，会话 Env 必须真的接上）" $ do
        ss <- okSteps ["def f x = x + 1;", "def g x = f x;", "g 1;"]
        stmtVal 0 (at 2 ss) `shouldBe` "2"

    it "跨批三级链 f → g → h" $ do
        ss <- okSteps ["def f x = x + 1;", "def g x = f x;", "def h x = g x;", "h 1;"]
        stmtVal 0 (at 3 ss) `shouldBe` "2"

    it "def 体能读到会话里 0 参 def 的值" $ do
        ss <- okSteps ["def k = 5;", "def g x = k + x;", "g 1;"]
        stmtVal 0 (at 2 ss) `shouldBe` "6"

    it "参数名遮蔽同名的外层 def：def b a = a + 1 里的 a 是参数" $ do
        ss <- okSteps ["def a x = x;", "def b a = a + 1;", "b 1;"]
        stmtVal 0 (at 2 ss) `shouldBe` "2"

    it "回归：用户报的 max2/max3 —— 两个 def 分两行，第三行调用" $ do
        ss <- okSteps
            [ "def max2 a b = if a < b then b else a;"
            , "def max3 a b c = if max2 a b == a then if max2 a c == a then a else c else if max2 b c == b then b else c;"
            , "max3 1 4 3;" ]
        stmtType 0 (at 2 ss) `shouldBe` "Int"
        stmtVal  0 (at 2 ss) `shouldBe` "4"

    it "互递归：even/odd 在同一个批里由同一个 SCC 一起定型并一起结绳" $ do
        -- 互递归的两个 def 必须同属一个 Program：逐行喂（REPL 那样）时
        -- odd 还没定义，even 的闭包自然看不到它 —— 这是会话语义，不是缺陷。
        -- （同一个批里它们互相可见，靠的仍是 knot 本身。）
        s <- okProg (T.unlines
            [ "def even n = if n == 0 then true else odd (n - 1);"
            , "def odd  n = if n == 0 then false else even (n - 1);"
            , "even 4;" ])
        stmtType 2 s `shouldBe` "Bool"
        stmtVal  2 s `shouldBe` "true"

    it "会话里重定义遮蔽旧定义（TEnv 与 Env 都要覆盖）" $ do
        ss <- okSteps ["def x = 1;", "def x = 2;", "x;"]
        stmtVal 0 (at 2 ss) `shouldBe` "2"
        -- 类型环境里也只剩一份 x，且是新的那次
        stmtType 0 (at 1 ss) `shouldBe` "Int"

    it "计数器跨行单调：第二行的类型变量编号不回到 0" $ do
        ss1 <- okSteps ["lambda a -> a;"]
        ss2 <- okSteps ["lambda a -> a;", "lambda b -> b;"]
        -- 显示是 a0（每行独立重编号）；真正的"跨行单调"看 prNext。
        stmtType 0 (at 1 ss2) `shouldBe` "(a0 -> a0)"
        prNext (at 1 ss2) `shouldSatisfy` (> prNext (at 0 ss1))

    it "一行里只要有类型错误，整行不提交（调用方保留旧会话）" $ do
        ss <- okSteps ["def x = 1;"]
        let s1 = at 0 ss
        progRun s1 "def x = 1 + true;" `shouldSatisfy` isLeft
        -- 旧的会话没被污染：x 仍然是 1
        case progRun s1 "x;" of
            Right s3 -> stmtVal 0 s3 `shouldBe` "1"
            Left e   -> expectationFailure ("s1 应当还能用：" <> T.unpack e)

    it "def 绑定的语法是 =，lambda 参数用 ->（两者不一致，见报告）" $ do
        progRun emptyProg "def f x = x + 1;"  `shouldSatisfy` isRight
        progRun emptyProg "def f x -> x + 1;" `shouldSatisfy` isLeft

  -------------------------------------------------------------------------
  describe "ADT：data / 构造子 / 构造子模式" $ do

    -- 这一组是 TODO 12（ADT 6 阶段）的回归网。写的时候 6 个阶段已经落地，
    -- 但**带参数**的 data 声明整个解析不出来（parseData 用裸 varName 收参数，
    -- 不吃尾随空格，于是 `data Maybe a = ...` 在 '=' 前的空格上炸），
    -- 而且 `a -> b` 这类以小写类型变量结尾的箭头类型也解析不出来
    -- （parseTypeName 的 varName 分支不吃尾随空格）。两条都是 1 token 的修法：
    --     ps <- many (lexeme varName)
    --     n  <- lexeme (upperName <|> varName)
    -- 修好之前，这一组里的「带参」用例会红 —— 那正是它们的用途。

    it "零参 data：构造子是值，且类型就是 data 名" $ do
        ss <- okSteps ["data Color = Red | Green;", "Red;"]
        stmtType 0 (at 1 ss) `shouldBe` "Color"
        stmtVal  0 (at 1 ss) `shouldBe` "Red"

    it "带参 data 能声明（当前红：parseData 的 varName 不吃空格）" $ do
        ss <- okSteps ["data Maybe a = None | Some a;", "Some 1;", "None;"]
        stmtType 0 (at 1 ss) `shouldBe` "Maybe Int"
        stmtVal  0 (at 1 ss) `shouldBe` "(Some 1)"

    it "构造子模式：match 能匹配构造子" $ do
        ss <- okSteps
            [ "data Color = Red | Green;"
            , "def f c = match c with Red -> 1 | Green -> 2;"
            , "f Green;" ]
        stmtType 0 (at 1 ss) `shouldBe` "(Color -> Int)"
        stmtVal  0 (at 2 ss) `shouldBe` "2"

    it "ADT 让多态 head 第一次写得出来（这是整件事的收益）" $ do
        ss <- okSteps
            [ "data Maybe a = None | Some a;"
            , "def head l = match l with [] -> None | x: xs -> Some x;"
            , "head [1];" ]
        stmtType 0 (at 1 ss) `shouldBe` "Forall a0. ([a0] -> Maybe a0)"
        stmtType 0 (at 2 ss) `shouldBe` "Maybe Int"

    it "构造子部分应用：还差参数时不报错，饱和后就是值" $ do
        ss <- okSteps ["data Pair a b = MkPair a b;", "MkPair 1;"]
        stmtType 0 (at 1 ss) `shouldBe` "(a0 -> Pair Int a0)"

    it "构造子模式数量不符要报错" $
        -- 必须写在同一个 progRun 里：跨行的 ctor 靠 prDEnv 传（见上面的 unionDEnv）
        progSteps ["data Maybe a = None | Some a;\ndef bad m = match m with Some x y -> x;"]
            `shouldSatisfy` \r -> case r of
                Left e  -> "expects 1 argument" `T.isInfixOf` e
                Right _ -> False

    it "未声明的构造子在模式里要报错" $ do
        progSteps ["data Color = Red;\ndef f c = match c with Blue -> 1;"]
            `shouldSatisfy` \r -> case r of
                Left e  -> "Unknown constructor" `T.isInfixOf` e
                Right _ -> False

    it "穷尽性：漏了构造子要报错" $ do
        progSteps ["data Color = Red | Green;\ndef f c = match c with Red -> 1;"]
            `shouldSatisfy` \r -> case r of
                Left e  -> "not exhaustive" `T.isInfixOf` e
                Right _ -> False

    it "重复的 data / 构造子名要报错" $ do
        progSteps ["data Color = Red | Green;\ndata Color = Blue;"]
            `shouldSatisfy` \r -> case r of
                Left e  -> "Duplicate data" `T.isInfixOf` e
                Right _ -> False
        progSteps ["data Color = Red;\ndata Other = Red;"]
            `shouldSatisfy` \r -> case r of
                Left e  -> "Duplicate constructor" `T.isInfixOf` e
                Right _ -> False

    -- occursIn 漏 TCon 分支的话（见 TODO 12 §5.2），这里不是报错而是**挂死**：
    -- apply 已经是递归的，循环替换会让求值转不出来。所以这条用例必须带超时跑，
    -- 挂死 = 红。
    it "occurs check：Node l l l 要报 Occurs check failed，不能挂死" $ do
        progSteps
            [ "data Tree a = Leaf | Node (Tree a) a (Tree a);\n\
              \def f x = match x with Leaf -> Leaf | Node l a r -> Node l l l;" ]
            `shouldSatisfy` \r -> case r of
                Left e  -> "Occurs check" `T.isInfixOf` e
                Right _ -> False

    it "标注 e :: T 的变量是新鲜的：不泄漏到外面" $ do
        ss <- okSteps ["def f x = x :: a;", "f 1;", "f true;"]
        stmtType 0 (at 0 ss) `shouldBe` "Forall a0. (a0 -> a0)"
        stmtType 0 (at 1 ss) `shouldBe` "Int"
        stmtType 0 (at 2 ss) `shouldBe` "Bool"

    it "标注写出了具体类型：对不上要报错" $ do
        progRun emptyProg "def f x = x :: Int;" `shouldSatisfy` isRight
        progRun emptyProg "1 :: Int;"           `shouldSatisfy` isRight
        runSrc "1 :: Bool" `shouldSatisfy` \o -> case o of
            TypeErr _ -> True
            _         -> False

  -------------------------------------------------------------------------
  describe "错误位置渲染（Span）" $ do

    it "未绑定变量：有插入符，且源行回显正确" $
        rendersCaret "zzz is an unbound variable." "zzz"

    it "插入符的列号 == 出错 token 在源码行里的列号" $ do
        let src = "1 + zzz"
            txt = case run src of
                    Right ast -> case tcRun ast of
                                    Left terr -> prettyTypeErrorWith "" src terr
                                    Right _   -> ""
                    Left _ -> ""
            ls = T.lines txt
        length ls `shouldBe` 3
        -- ls!!0 = "1 | 1 + zzz"（源码行回显）, ls!!1 = "  |     ^"
        T.findIndex (== '^') (ls !! 1) `shouldBe` T.findIndex (== 'z') (ls !! 0)

  -------------------------------------------------------------------------
  -- 下面这些断言的是【当前错误的行为】，所以今天是绿的。
  -- 哪天变红了 = 缺陷被修好，请把对应条目搬到上面「正确行为」去。
  -------------------------------------------------------------------------
  -- 递归与终止
  --
  -- 2026-09-23 补。起因：用户报 `def f n = if n == 0 then 1 else f n` 之后
  -- `f 1` 不终止。查下来是**深度守卫没接在递归路径上** —— evalwDepth 只包在
  -- 语句层，App 求值函数体走的是裸 eval，所以 maxDepth 永远够不着。
  -- 后果不只是"卡住"：实测失控递归以 ~250 MB/s 吃内存（6 秒 1.5 GB、
  -- 14 秒 3.3 GB），会把机器拖死。
  --
  -- 这一组只断言「合法递归必须正常终止」—— 有没有守卫它们都该绿。用途是
  -- 挡住"加守卫时把 maxDepth 设得太低、误伤真实程序"这个反向错误
  -- （实测把上限设成 1000 时，count 500 就会被误判成 RecursionLimited）。
  --
  -- 失控递归本身必须有超时才测得动，放在 test/run-golden.sh 的体检里，
  -- 不放这儿：hspec 里一个跑飞的用例会先把内存吃光。
  -------------------------------------------------------------------------
  describe "递归与终止" $ do

    it "阶乘" $ do
        s <- okProg (T.unlines
            [ "def fact n = if n == 0 then 1 else n * fact (n - 1);"
            , "fact 10;" ])
        stmtVal 1 s `shouldBe` "3628800"

    it "斐波那契" $ do
        s <- okProg (T.unlines
            [ "def fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);"
            , "fib 15;" ])
        stmtVal 1 s `shouldBe` "610"

    it "尾递归 5000 层" $ do
        s <- okProg (T.unlines
            [ "def count n = if n == 0 then 0 else count (n - 1);"
            , "count 5000;" ])
        stmtVal 1 s `shouldBe` "0"

    it "互递归 5000 层" $ do
        s <- okProg (T.unlines
            [ "def even n = if n == 0 then true else odd (n - 1);"
            , "def odd  n = if n == 0 then false else even (n - 1);"
            , "even 5000;" ])
        stmtVal 2 s `shouldBe` "true"

    it "高阶：把递归函数当参数传来传去" $ do
        s <- okProg (T.unlines
            [ "def applyN f n x = if n == 0 then x else applyN f (n - 1) (f x);"
            , "applyN (lambda y -> y + 1) 1000 0;" ])
        stmtVal 1 s `shouldBe` "1000"

    it "let 绑定的递归函数" $ do
        s <- okProg "let lf = lambda n -> if n == 0 then 0 else lf (n - 1) in lf 500;"
        stmtVal 0 s `shouldBe` "0"

    it "递归函数跨行定义后仍可调用（会话 Env 接得上）" $ do
        ss <- okSteps ["def count n = if n == 0 then 0 else count (n - 1);", "count 1000;"]
        stmtVal 0 (at 1 ss) `shouldBe` "0"

  describe "已知缺陷（断言当前行为，修好后请搬走）" $ do

    -- 曾经这里还有一条 `let x = x in 1 类型检查放行`，2026-09-24 修好后
    -- 已翻面成正向断言，搬去上面的「let 求值」组。

    it "互递归只有 def 支持；let 仍然不行（let 不是互递归结点）" $
        runSrc "let even = lambda n -> if n == 0 then true else odd (n - 1) in even 4"
            `shouldSatisfy` \o -> case o of
                TypeErr t -> T.isInfixOf "odd is an unbound variable" t
                _         -> False

    -- 2026-09-27：原来这里还有一条「if / let 少分支误报 reserved symbol」，
    -- 缺陷已修好，翻面成正向断言搬去「模式匹配」组（见那组最后三条）。

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
                === Ok "Bool" (if a < b then "true" else "false")

    it "括号包裹不改变语义" $
        property $ forAll (choose (-1000, 1000) :: Gen Integer) $ \a ->
            runSrc (T.pack (show a)) === runSrc ("(" <> T.pack (show a) <> ")")
