# interp

一个用 Haskell 编写的小型函数式语言解释器。项目实现了从解析、Hindley–Milner 类型推断到求值的完整流水线，并提供交互式 REPL、源码文件执行和一个用解释器自身编写的 Prelude。

> 仓库目录名目前是 `interepter`，Cabal 包和生成的可执行文件名则是 `interp`。

## 已实现的能力

- `Int`、`Bool`、`String`、列表和元组
- `let`、`if`、柯里化函数与词法闭包
- 自动的多态类型推断，以及 `::` 类型标注
- 递归、互递归和定义依赖检查
- 代数数据类型（ADT）与穷尽性模式匹配
- 类型别名
- 记录构造、字段访问、记录更新和记录模式
- 自定义中缀运算符及其优先级、结合性
- 带别名的文件导入
- 带源码位置的解析、类型和求值错误
- 由 `utils/prelude.txt` 提供的常用列表、数字、字符串和组合子函数

这个项目仍处于开发阶段，不以兼容 Haskell 为目标；语法只是有意采用了一部分 Haskell / ML 风格。

## 环境要求

- GHC
- Cabal（支持 `cabal-version: 3.0`）

依赖由 Cabal 管理，主要包括 `text`、`containers`、`mtl`、`megaparsec` 和 `parser-combinators`；测试还使用 `hspec` 与 `QuickCheck`。

## 构建与运行

在仓库根目录执行：

```bash
cabal build
cabal run interp
```

启动后会进入 REPL：

```text
HM interpreter. Type :q to quit.
> 1 + 2 * 3
Type : Int
Value: 7
> :t lambda x -> x
Type : (a0 -> a0)
```

也可以执行源码文件：

```bash
cabal run interp -- path/to/program.txt
```

程序启动时会尝试读取相对路径 `utils/prelude.txt`。为了正常加载 Prelude，请从仓库根目录启动可执行文件；如果 Prelude 不存在或不可读，解释器仍会启动，但其中定义的函数和数据类型不会可用。

## REPL 命令

| 命令 | 说明 |
| --- | --- |
| `:help` | 显示帮助 |
| `:t <expr>` | 只推断表达式类型，不求值 |
| `:load <file>` | 将文件加载到当前会话 |
| `import <file> as <Alias>` | 导入文件，并使用大写别名限定其中的名字 |
| `:q` | 退出 |

REPL 会保留此前成功提交的定义、数据类型、类型别名、记录字段和运算符声明。

## 语言速览

程序由以分号结尾的声明或表达式组成。`--` 可用于整行注释；当前不支持把注释放在代码末尾。

### 表达式与函数

```text
def square x = x * x;
def factorial n = if n == 0 then 1 else n * factorial (n - 1);

square 9;
factorial 10;
(lambda x y -> x + y) 20 22;
let (x, y) = (2, 3) in x ^ y;
```

函数应用用空格表示，并采用柯里化语义。整数使用任意精度的 Haskell `Integer`。

### 列表与模式匹配

```text
def length xs = match xs with
      [] -> 0
    | _ : rest -> 1 + length rest
    ;

map (lambda x -> x * 2) [1, 2, 3];
```

构造列表可以使用 `[1, 2, 3]`、`1 : 2 : []` 或内置的 `cons`。类型检查器会检查
模式是否穷尽，并拒绝重复的模式变量。

### ADT、类型别名与记录

```text
type Name = String;

data Shape
    = Circle { radius :: Int }
    | Rectangle { width :: Int, height :: Int };

def areaLike s = match s with
      Circle { radius = r } -> r * r
    | Rectangle { width = w, height = h } -> w * h
    ;

def widen s = s { width = s.width + 1 };
```

普通位置式构造子同样受支持：

```text
data Tree a = Leaf | Node a (Tree a) (Tree a);
```

### 自定义运算符

```text
def (<+>) a b = a + b;
infixl 6 <+>;

1 <+> 2 <+> 3;
```

支持 `infixl`、`infixr` 和非结合的 `infix`，优先级必须是 `0` 到 `9` 的一位数字。内置运算符的优先级不可重定义。

### 文件导入

```text
import "utils/data/map.txt" as M

def numbers = M.fromListInt [(2, "two"), (1, "one")];
M.lookup 2 numbers;
```

`as` 不可省略，别名必须以大写字母开头。相对导入路径以当前被加载文件所在目录为基准解析；同一个模块中的值、类型和构造子通过别名限定。

## Prelude

`utils/prelude.txt` 随解释器自动加载，其中包括：

- `Maybe`、`Either`、`Ordering` 等基础数据类型
- `map`、`filter`、`foldl`、`foldr`、`zip`、`take`、`drop` 等列表函数
- `min`、`max`、`gcd`、`even`、`odd` 等整数函数
- `id`、`compose`、`curry`、`uncurry` 等组合子
- `strLen`、`strConcat`、`strReverse` 等字符串函数

完整定义请直接查看 [`utils/prelude.txt`](utils/prelude.txt)。`utils/data/` 中还有使用该
语言实现的 `Map` 和 `Set` 示例。

## 测试

运行 Cabal 测试套件：

```bash
cabal test
```

仓库还提供了两个便于直接运行的脚本：

```bash
bash test/run-spec.sh
bash test/run-golden.sh
```

测试覆盖解析、类型推断、求值、递归、模式匹配、ADT、导入、字符串、自定义运算符和记录等行为。`test/known-bugs.txt` 与 `test/known-bugs.current.txt` 记录了已知问题的基线和当前结果。

## 项目结构

```text
app/Main.hs              命令行入口、REPL、文件加载与导入
src/Interp/Parser.hs     解析及记录语法的去糖
src/Interp/TypeCheck.hs  HM 类型推断与模式检查
src/Interp/Eval.hs       求值器
src/Interp/Pretty.hs     值、类型和诊断信息的格式化
src/Interp/Builtin.hs    内置值、类型与初始会话
src/Interp/Qualify.hs    导入模块的名字限定
src/Interp/Synonym.hs    类型别名展开
src/Interp/Types.hs      AST、类型、值及运行时状态
utils/prelude.txt        用目标语言编写的 Prelude
utils/data/              Map / Set 示例模块
test/                    Hspec、QuickCheck 与 golden tests
```

开发计划和历史审查记录位于 `TODO/`。
