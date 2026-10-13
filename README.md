# interp

用 Haskell 实现的 HM 风格函数式语言：ADT、Record、模式匹配、自定义运算符、隐式参数、递归字典求解和效果跟踪。目录名为 `interepter`，Cabal 包和可执行文件名为 `interp`。

本项目用于探索语言设计与改善实际编程体验，不以兼容 Haskell 或立即工业化为目标。

## 文档入口

- [语言参考](docs/LanguageReference.md)：当前语法、类型、隐式搜索、求值与效果规则。
- [设计记录](docs/DesignNotes.md)：功能初衷、目的、代价、决策及尚未确认的方案。
- [当前 TODO](TODO/archive/README-before-docs-20261012.md)：近期路线、实验与验收。
- [历史归档](TODO/archive/README.md)：过去的代码交付、验证证据和被替代计划。

更新基线：2026-10-12，HEAD `b602833`，包版本 `0.1.0.0`，包含当时用户的工作区 Prelude 改动。主源码已含 R5；本轮维护文档，没有重新运行主版本完整验收。历史副本测试数字不能代表今天的主工作区。

## 构建和运行

需要 GHC 与 Cabal，依赖见 [interp.cabal](interp.cabal)。从项目根目录执行，以便加载 `utils/prelude.txt`：

```bash
cabal build
cabal run interp
cabal run interp -- example.txt
```

REPL 支持 `:help`、`:t expr`、`:load path`、`:implicits`、`:trace`、`import "path" as M` 和 `:q`。`:t` 只检查类型/效果，不执行输出。

```text
def square x = x * x;
square 9;
putStrLn (show 81);
```

当前 Prelude 用 `show` 展示值，`showInt` 是字典，不能继续照抄旧示例里的 `showInt n`。行尾 `--` 注释已经支持；尚未加入的 ability、do、guard、字段多态等方案请看设计记录。

## 测试

```bash
cabal test
bash test/run-spec.sh
bash test/run-golden.sh
```

当前测试文件仍有旧深度/计数器接口和旧 Prelude 用法，迁移与主版本验收是 [P0 工作](TODO/32-当前路线与验收.md)。这些命令是项目测试入口，不表示本轮已执行或当前全部通过。

## 结构

```text
src/Interp/   AST、解析、类型与效果推断、求值、模块限定、类型别名
app/Main.hs   REPL、文件执行、导入、输出事件重放
utils/        用户维护的 Prelude 和语言自身编写的库
test/         Hspec、性质测试与金标
docs/         当前语言参考与设计决策
TODO/         活跃计划与历史归档
```

保护目录与持续文档维护规则见 [AGENTS.md](AGENTS.md)。
