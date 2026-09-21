{-# LANGUAGE OverloadedStrings #-}

-- | interp 的 hspec 测试套件。
--
-- == 运行前提（需要你手动改一行，不在本次交付范围内）
--
-- interp.cabal 的 test-suite 目前是：
--
-- > build-depends: base, interp, hspec, QuickCheck
--
-- 本文件要用 Data.Text / Data.Map / runReader / runState，所以要补三个包：
--
-- > build-depends: base, interp, hspec, QuickCheck, text, containers, mtl
--
-- 在补上之前 `cabal test` 无法编译本文件。
-- 在此期间的回归保护由 test/run-golden.sh 提供（它不需要改 cabal）。
--
-- == 设计说明
--
-- Types.hs 里的 V' / EvalError 目前没有任何 deriving（见 TODO/01-近期.md 第 1 项），
-- 所以这里不直接比较值，而是比较 Pretty.hs 各函数的 Text 输出。
-- 好处是本文件对**当前**代码就能编译通过，不必先等 deriving 那步；
-- 等 TODO/01 做完后，可以改成直接对 V' 做 shouldBe，断言会更硬。
--
-- "已知缺陷"那一节断言的是**当前的错误行为**，作用是：
-- 缺陷一旦修好，这些用例会失败并提醒你更新它们。
module Main (main) where

import Interp.Types
import Interp.Parser (run)
import Interp.Eval (eval)
import Interp.TypeCheck (typeChecker)
import Interp.Pretty

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.State (runState)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, forAll, (===))

-- ---------------------------------------------------------------------------
-- 把整条流水线（解析 -> 类型检查 -> 求值）封成一个可断言的结果
-- ---------------------------------------------------------------------------

data Outcome
    = ParseErr Text
    | TypeErr Text
    | EvalErr Text
    | Ok Text Text      -- ^ 类型, 值
    deriving (Show, Eq)

-- | 类型检查的 monad 栈需要显式钉住：typeChecker 的签名是
-- @(MonadState Counter m, MonadError TypeError m)@，两个约束都是裸类型变量，
-- GHC 无法自行推断该用哪个栈（会报 Couldn't match ... StateT s Identity）。
tcRun :: E' -> (Either TypeError (TSub, T'), Counter)
tcRun e = runState (runExceptT (typeChecker M.empty e)) 0

-- | 同上，求值这里 @m = ExceptT EvalError (Reader Env)@。
evRun :: E' -> Either EvalError V'
evRun e = runReader (runExceptT (eval e)) M.empty

runSrc :: Text -> Outcome
runSrc src = case run src of
    Left perr -> ParseErr perr
    Right ast -> case tcRun ast of
        (Left terr, _)     -> TypeErr (prettyTypeError terr)
        (Right (_, ty), _) -> case evRun ast of
            Left eerr -> EvalErr (prettyEvalError eerr)
            Right v   -> Ok (prettyT' ty) (prettyV' v)

-- | 大多数用例结果都是 Int，省点字。
val :: Text -> Outcome
val v = Ok "Int" v

-- | 断言某个 parse error 的文本里包含给定片段（parse error 是多行的，
-- 不适合整串比较）。
mentionsParseErr :: Text -> Outcome -> Bool
mentionsParseErr needle (ParseErr t) = needle `T.isInfixOf` t
mentionsParseErr _ _ = False

-- ---------------------------------------------------------------------------

main :: IO ()
main = hspec $ do

    describe "解析器：优先级与结合性（回归基线，不应改变）" $ do
        it "乘法高于加法"         $ runSrc "1 + 2 * 3" `shouldBe` val "7"
        it "幂右结合"             $ runSrc "2 ^ 3 ^ 2" `shouldBe` val "512"
        it "减法左结合"           $ runSrc "1 - 2 - 3" `shouldBe` val "-4"
        it "一元负号低于幂"       $ runSrc "-2 ^ 2"    `shouldBe` val "-4"
        it "括号可改变优先级"     $ runSrc "(-2) ^ 2"  `shouldBe` val "4"
        it "中缀负号接一元负号"   $ runSrc "2 - -3"    `shouldBe` val "5"
        it "整数除法向负无穷取整" $ runSrc "10 / 3"    `shouldBe` val "3"

    describe "求值：let / lambda / 柯里化" $ do
        it "let"                  $ runSrc "let x = 3 in x + 1" `shouldBe` val "4"
        it "嵌套 let"             $
            runSrc "let a = 1 in let b = 2 in a + b * 3" `shouldBe` val "7"
        it "lambda 应用"          $ runSrc "(lambda x = x * 2) 3" `shouldBe` val "6"
        it "多参数柯里化"         $
            runSrc "let k = lambda x = lambda y = x in k 1 2" `shouldBe` val "1"
        it "闭包捕获定义处环境"   $
            runSrc "(lambda x = let y = 5 in lambda z = x - y + 2 * z) 2 7"
                `shouldBe` val "11"

    describe "求值：词法作用域" $ do
        -- 这条是本套件里最有价值的一个用例：如果求值改成动态作用域，
        -- 它给出的会是 2 而不是 1。
        it "内层 let 遮蔽后，闭包仍看定义处的 a" $
            runSrc "let a = 1 in let f = lambda u = a in let a = 2 in f 0"
                `shouldBe` val "1"

    describe "类型推导" $ do
        it "恒等函数得到 a0 -> a0" $
            runSrc "lambda x = x" `shouldBe` Ok "(a0 -> a0)" "<closure x -> ...>"
        it "let 多态：同一 id 可用于不同类型" $
            runSrc "let id = lambda x = x in id 5" `shouldBe` val "5"
        it "occurs check 拒绝自应用" $
            runSrc "lambda x = x x" `shouldBe` TypeErr "Ocuur type check."

    describe "错误处理" $ do
        it "未绑定变量" $ runSrc "y" `shouldBe` TypeErr "y is an unbound variable."
        it "除零"       $ runSrc "1 / 0" `shouldBe` EvalErr "Divided by zero at 0."

    -- -----------------------------------------------------------------------
    -- 以下断言的是当前**错误**行为。修好任一项后，对应用例会失败 ——
    -- 那时请更新断言，并把这次修复从 test/known-bugs.current.txt 里移除。
    -- -----------------------------------------------------------------------
    describe "已知缺陷（断言当前错误行为，修好后请更新）" $ do
        it "Int 静默溢出：2^100 得到 0（应为 1267650600228229401496703205376）" $
            runSrc "2 ^ 100" `shouldBe` val "0"

        it "Int 静默溢出：2^63 得到负数" $
            runSrc "2 ^ 63" `shouldBe` val "-9223372036854775808"

        it "整数字面量静默溢出（词法层，与上面同一个根因的另一处）" $
            runSrc "99999999999999999999999" `shouldBe` val "200376420520689663"

        it "缺词法边界：123abc 被解析成函数应用，报错指向 abc" $
            runSrc "123abc" `shouldBe` TypeErr "abc is an unbound variable."

        it "类型不匹配消息有拼写错误且句点前多一个空格" $
            runSrc "1 + (lambda x = x)" `shouldBe` TypeErr "Type mismath, Int vs Function ."

        it "空格应用 + 中缀负号的歧义：(lambda f = f) -2 被当成减法" $
            runSrc "(lambda f = f) -2" `shouldBe` TypeErr "Type mismath, Int vs Function ."

        it "let x = 1 in(x) 的报错指向了正确的 token let（真实原因是 in 后缺空格）" $
            runSrc "let x = 1 in(x)"
                `shouldSatisfy` mentionsParseErr "reserved keyword: \"let\""

    describe "属性测试" $ do
        prop "加法在 解析+类型检查+求值 全链路上成立" prop_add
        prop "恒等函数对任意整数返回原值"             prop_identity

-- ---------------------------------------------------------------------------
-- QuickCheck
-- ---------------------------------------------------------------------------

-- 限定范围，避免撞上 Int 溢出（溢出是已知缺陷，见上）
smallInt :: Gen Int
smallInt = choose (-1000, 1000)

prop_add :: Property
prop_add = forAll smallInt $ \a -> forAll smallInt $ \b ->
    runSrc (T.pack (show a) <> " + " <> T.pack (show b))
        === val (T.pack (show (a + b)))

-- 注意这里给实参加括号：`(lambda x = x) -1000` 会被解析成减法
-- （已知缺陷，见上面那条用例），加括号才能表达"把 -1000 作为参数"。
prop_identity :: Property
prop_identity = forAll smallInt $ \a ->
    runSrc ("(lambda x = x) (" <> T.pack (show a) <> ")")
        === val (T.pack (show a))
