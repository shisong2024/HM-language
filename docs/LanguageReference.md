# Language Reference：当前语言参考

更新：2026-10-12。基于 HEAD `b602833` 及当前工作区；具体边界见[文档入口](README.md)。本文件描述当前行为，未来方案见 [Design Notes](DesignNotes.md)。

本轮状态为源码核对，不声称下列示例已在当前主版本重新执行。R0–R5 已有主源码实现标志，但主工作区完整验收尚待完成。

## 1. 运行与程序组织

Cabal 包与可执行文件名为 `interp`，目录名为 `interepter`。从项目根目录运行：

```bash
cabal build
cabal run interp
cabal run interp -- example.txt
```

启动时读取相对路径 `utils/prelude.txt`。`Unit`、`Maybe`、`Ord`、`Show` 等类型及常用函数由当前 Prelude 提供，不能当作都由类型检查器内建。

程序声明与表达式通常以 `;` 结束。REPL 的 `:t` 接受省略末尾分号。空白不构成缩进语法；换行本身不是块边界。`--` 注释可出现在行首或代码末尾，字符串中的 `--` 不启动注释。

| REPL 命令 | 含义 |
|---|---|
| `:help` | 帮助 |
| `:t expr` | 推断类型及立即效果，不执行表达式 |
| `:load path` | 加载到当前会话 |
| `import "path" as M` | 以大写别名导入 |
| `:implicits` | 列出会话候选，提示潜在 head 重叠 |
| `:trace` | 显示上一次求值批次的输出事件，不重放输出 |
| `:q` | 退出 |

当前 `:t` 类型标签显示为 `Type:`，普通批次求值输出使用 `Type :`。这些是显示格式，不是语言语法。

## 2. 值、函数与表达式

基础类型包括任意精度整数 `Int`、`Bool`、`String`、同质列表 `[a]` 和异构元组 `(a, b, ...)`。`Unit` 当前通过 Prelude 的 `data Unit = Unit;` 提供，使用 `Unit` 值。

```text
def square x = x * x;
def choose b x y = if b then x else y;
def identity x = x;
(lambda x y -> x + y) 2 3;
let (x, y) = (2, 3) in x + y;
```

函数以空格应用，支持柯里化与部分应用。普通函数参数、lambda 参数是名字；模式函数参数及多条同名函数方程尚未支持。顶层定义按依赖分组处理，支持递归与互递归。

`let` 的模式必须不可失败，例如变量、通配符或由它们组成的元组。可能失败的列表/构造器模式应使用 `match`。普通局部 lambda 绑定可以递归；不要据此推断任意局部值都支持安全递归。

核心求值器按值调用：函数与实参先求值，元组/列表元素按顺序求值，`if` 只执行所选分支。但隐式 elaboration 会改变表达式形状：带字典约束的绑定可能变为隐藏字典函数，执行绑定时只是构造闭包，实际计算发生在补入字典后的使用处。第 6 节说明这个区别。

## 3. 类型推断与注解

语言采用 HM 风格的自动泛化与实例化。每次使用多态定义都会独立实例化量化变量。类型构造器可应用于类型，例如 `Maybe Int`、`Ord [Int]`；当前不支持类型变量应用 `f a`，没有 HKT 或局部显式 `forall` 字段。

```text
def asInt x = x :: Int;
(lambda x -> x + 1) :: Int -> Int;
```

类型注解使用 `::`。必要时用括号界定被注解表达式。没有单独的顶层函数签名语法，也没有 `(x :: Int)` 形式的显式函数参数声明。

同一个 `def` 的隐式参数与 body 注解中，同名源变量 `a` 共享；不同定义独立。`let` 绑定值 RHS 内的注解保留局部范围，不参加外层定义的共享，`in` 后续表达式仍属于外层 body。裸注解中常用单个小写字母作为变量；`data`/`type` 声明的显式参数表另有名字绑定。

```text
def same {o1 :: Ord a} {o2 :: Ord a} x y =
    (o1.cmp x x, o2.cmp y y);
-- x、y 的类型相同。

def independent {o1 :: Ord a} {o2 :: Ord b} x y =
    (o1.cmp x x, o2.cmp y y);
-- a、b 独立。
```

注解中的类型变量属于当前推断机制；不要将它等同于 higher-rank 或显式刚性全称量化。

## 4. ADT、模式与 Record

```text
data Tree a = Leaf | Node a (Tree a) (Tree a);
type Name = String;
data Person = Person { personName :: String, personAge :: Int };

def age p = p.personAge;
def older p = p { personAge = p.personAge + 1 };
def label p = match p with Person { personName = n } -> n;
```

支持位置构造器、Record 构造、字段读取、不可变字段更新、Record 模式、列表与元组模式。模式检查包含重复变量与穷尽性检查；各构造器分支应覆盖可能输入。

当前字段表为 `Map Text FieldInfo`，字段访问在解析后的去糖阶段选择具体构造器并转成 `Match`。同一可见字段表中，不能把同名字段作为多个不同 Record 的独立字段。导入与重载有专门限定/合并逻辑，尚不是类型驱动的同名字段解析。

字段属于某个构造器。多构造器 ADT 的一个字段可能不在所有构造器中存在；读取或更新不含该字段的构造器会产生运行期字段错误。因此“类型知道所属 ADT”不等于“访问在所有值上都成功”。

嵌套 `match` 的 `|` 可能被内层消费，必要时用括号隔开。当前没有 `end`、花括号 match block、guard 或缩进块语法。

## 5. 运算符

```text
def (|>) x f = f x;
infixl 1 |>;
3 |> square;
(+) 1 2;
(+ 1) 2;
(1 +) 2;
```

支持自定义运算符、引用以及左右段。`infixl`、`infixr`、`infix` 指定结合性，优先级为 0–9 的单个数字。语法保留的 `=`、`->` 等不作为普通可定义运算符；内建运算符 fixity 不可随意重定义。

算术主要作用于整数。`==`、`!=` 是内建比较机制，不通过用户定义的 `Eq` 字典。当前 `<`、`>`、`<=`、`>=` 的通用排序行为由 Prelude 的 `Ord` 字典定义。

## 6. 隐式参数与字典

当前能力接口仍是普通 `data`：

```text
data Ord a = Ord { cmp :: a -> a -> Ordering };
data Show a = Show { show' :: a -> String };
```

这些是当前 Prelude 的定义形式，不需要再次在加载了 Prelude 的会话里声明。当前没有 `ability`/`trait` 关键字，也没有“仅能力类型可隐式传递”的限制。普通数据类型和基础类型仍可能作为隐式参数，只需满足现有候选检查。

字典是一等值，可以由普通 `def` 保存、显式传给函数、从函数返回。`implicit def` 将字典或字典工厂注册到搜索空间。只构造一个普通字典值，不会自动注册。

```text
def compareWith {o :: Ord a} x y = o.cmp x y;
def reverseIntOrder = Ord { cmp = lambda x y -> ordInt.cmp y x } :: Ord Int;
compareWith 1 2;
compareWith @{o = reverseIntOrder} 1 2;
```

`@{o = expr}` 精确补入具名槽位，普通参数仍按函数应用传递。重复、未知、类型不匹配或同名多槽位无法唯一识别时会报错。具名接口当前需要可识别的调用头；不能假设任意高阶参数 `f` 都支持 `f @{o=...}`。

未使用的隐式参数可以被去除。被使用的具名约束会保留并向调用者传播，即使类型已经具体，也不会无条件提前选取默认字典。

```text
def renderPair x y = (show x, show y);
```

两次 `show` 分别实例化，因此 x、y 可以不同类型，对应约束自动外传。若外传后出现同名但不同类型的字典槽位，默认填充可分别按类型处理；显式 `@{s=...}` 则可能因无法唯一指定槽位而歧义。

### 搜索规则

1. 具名 wanted 优先在原作用域帧中查找同名候选；同名存在但不匹配时不默默换成别的候选。找不到同名才走类型匹配。
2. 从内层帧向外搜索。某层零匹配才继续；一匹配选定；多匹配报歧义。
3. 同层不采用“更具体优先”。父候选选定后才求解其上下文，不根据子目标能否成功给父候选排名或回溯选择。
4. 候选变量独立刷新并单向匹配。搜索不决定 wanted 的普通类型变量；仍不确定时约束外传或在实际调用处报错。
5. 子字典递归构造为普通 `App` 表达式。使用原 wanted 的作用域，保留调用位置。

批次声明时的内层帧与持久会话帧不同。加载/导入后的候选会合并到会话；不能假定新候选永久屏蔽旧默认值。

### 候选限制

上下文变量必须出现在结果 head 中；直接依赖自身结果 head 的候选拒绝。递归搜索检测具体目标的路径循环，并限制为 64 层，以阻止持续增长的目标挂死。

自动字典构造不得产生立即效果；可以保存带固定效果标签的函数字段。候选 head/context 中的开放效果变量或残余效果关系暂不支持。这不禁止普通高阶函数的效果多态。

### 带约束绑定的求值

当前 `closeBinding` 可将约束变为隐藏 lambda。例如：

```text
let u = putStrLn (show 1) in 2;
```

这里 `show` 带具名字典约束，`u` 可能成为字典函数；未使用 `u` 就不执行其输出。这是目前从源码能够解释的行为，本轮未执行此示例。它不同于没有隐式约束的普通严格 let，也是未来顺序块必须明确处理的边界。

## 7. 效果

```text
def greet s = putStrLn s;
-- String -> {Console} Unit

(lambda s -> putStrLn s) :: String -> {Console} Unit;
```

箭头上的集合描述调用可能产生的潜在效果；表达式另有立即效果。取出函数或构造闭包不自动执行其 body；调用会将函数潜在效果和求值实参的立即效果传播出去。分支/模式分支采用保守合并，即使运行时某个分支未执行，其效果仍可能出现在静态集合中。

未写效果标签的箭头表示纯上界，例如 `String -> Unit` 不接受需要 Console 的函数。纯函数可以满足允许额外固定标签的上界。普通函数参数/返回值位置有相应方向检查，不能把效果包含当成无条件对称相等。

内部支持效果变量和高阶传播，例如 map 的回调效果可以传播到 map 的执行箭头。用户当前能在注解中写固定的大写标签集合；没有显式效果变量绑定/forall 语法。完整的开放集合约束求解能力仍有限，不把 R4 说成任意效果类型系统。

`putStr`、`putStrLn` 引入 Console，求值器记录输出事件，再由 Main 重放。`:t` 不执行这些操作；错误之前已经产生的事件可以保留。空效果集合只表示没有被追踪的效果，不保证终止、不保证不会除零或发生运行期错误。

效果标签没有自动对应的字典值。用户写 `{FileIO}` 不会创建文件操作或 handler。当前没有语言级可恢复异常、可变状态操作、algebraic effect handlers。

## 8. Prelude 与 Tuple

当前工作区的入口为 `show`，`Show` 字段名为 `show'`；`showInt` 已是 `Show Int` 字典，整数格式化函数为 `_showInt`。不要继续将旧文档中的 `showInt n` 当作当前接口。

当前还存在 Ord/Show 的列表、Maybe、Either 等实例，以及二元组 Show 实例。`showStr` 使用 `id`，字符串展示目前不会自动添加引号或转义；“展示字符串必须可重新解析”不是现有保证。

元组按固定元数区分类型，二元 `(a,b)` 与三元 `(a,b,c)` 不同。普通 HM 实例不能一次覆盖所有元数；二元嵌套 `(a,(b,c))` 可以递归组合二元实例，但不等同于三元元组。实例是否存在以当前 Prelude 为准。

Prelude 正在演进；其未提交改动属于用户。本轮文档维护不改写 Prelude。

## 9. 导入、诊断与当前缺口

文件导入写作 `import "path" as M;`，通常独占一行。别名必须大写起始，`Prelude` 保留。相对路径按当前加载文件目录解析，模块值/类型/构造器通过限定名使用。不要将它等同于拥有签名、私有成员或抽象类型的参数化模块系统。

已有源码位置、定义/调用说明、隐式缺候选/歧义、效果不匹配与求值错误。类型检查器有局部恢复机制，但当前 Main 遇到一个坏定义时可能拒绝整批提交；“能够继续检查”不等于“其余定义一定成功提交”。

当前没有同名字段多态、do、guard、模式函数方程、能力关键字、完整 row polymorphism、HKT、higher-rank 字段、GAT 或 handlers。讨论与原型不等于当前语言特性。

源码对应入口：[Parser](../src/Interp/Parser.hs)、[TypeCheck](../src/Interp/TypeCheck.hs)、[Eval](../src/Interp/Eval.hs)、[Types](../src/Interp/Types.hs)、[Main](../app/Main.hs)、[Prelude](../utils/prelude.txt)。历史测试证据见 [归档](../TODO/archive/README.md)，当前验收工作见 [路线](../TODO/32-当前路线与验收.md)。
