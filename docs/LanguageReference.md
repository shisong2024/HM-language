# Language Reference：当前语言参考

更新：2026-10-13。主版本基线为 HEAD `594df6b` 及当前工作区；具体边界见[文档入口](README.md)。未来方案与决策见 [Design Notes](DesignNotes.md)。

主源码已采用 R0–R5 与 ability/固定 effect 基础版。本轮冻结快照的解释器编译通过，接口适配后的基线测试为 332/0；原始主测试仍有旧导入/API，不能直接把这个数字视为未适配主测试的验收结果。

第 6、7 节描述已采用的基础版。第 10 节另列[参数化 effect/handler 隔离交付](../TODO/36-参数化Effect与深层Handler完整代码.md)，供审查和使用副本；该扩展尚未应用到主源码。

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
    (o1.cmp' x x, o2.cmp' y y);
-- x、y 的类型相同。

def independent {o1 :: Ord a} {o2 :: Ord b} x y =
    (o1.cmp' x x, o2.cmp' y y);
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

## 6. Ability、隐式参数与字典

基础版用 `ability` 声明可参与隐式传递的接口：

```text
ability Ord a = Ord { cmp' :: a -> a -> Ordering };
ability Show a = Show { show' :: a -> String };
ability Writer { emit :: String -> {Console} Unit };
```

前两条是当前 Prelude 定义形式，不需要在已加载 Prelude 的会话里重复声明。第三条省略 `= Writer`，构造器默认与能力同名。能力只能有一个 Record 构造器，或使用 `ability Proof = Proof;` 声明无字段标记；不支持多个构造器和位置载荷。字段可以是方法，也可以是普通配置值。方法不会自动导出为全局函数，仍通过字典字段访问。

隐式参数的类型必须是已声明能力的完整应用，例如 `Ord Int`、`Show a`；`implicit def` 的结果和隐式上下文也必须满足此规则。能力的类型别名可以使用，普通 `data` 包装能力不会自动获得资格。`Int`、普通数据类型、函数和裸类型变量不能作为隐式参数，未使用的源隐式参数也会被检查。普通 data 的隐式使用已在基础版采用时收紧；需要普通数据依赖时使用显式参数，或设计一个能力包装。

字典是一等值，可以由普通 `def` 保存、显式传给函数、从函数返回。`implicit def` 将字典或字典工厂注册到搜索空间。只构造一个普通字典值，不会自动注册。

普通数据仍使用 `data`，可以通过普通参数显式传递。Ability 复用现有 Record 构造、模式匹配与字段访问机制，仍受同名字段限制。同一存活作用域不能将已有类型从 `data` 改成 `ability`，或反向改变。

```text
def compareWith {o :: Ord a} x y = o.cmp' x y;
def reverseIntOrder = Ord { cmp' = lambda x y -> ordInt.cmp' y x } :: Ord Int;
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
effect Audit;
def mark x = x :: Int;
(lambda x -> x) :: Int -> {Audit} Int;

def greet s = putStrLn s;
-- String -> {Console} Unit

(lambda s -> putStrLn s) :: String -> {Console} Unit;
```

基础版通过 `effect Name;` 声明固定静态效果标签。`Console` 内建可用，显式写 `effect Console;` 也允许。同批重复声明会报错，跨批再次声明同一标签允许。类型与效果使用不同命名空间，效果声明不生成值、构造器、操作或字典。上面的纯函数可满足 `{Audit}` 上界，但声明 Audit 本身不产生任何实际操作。

注解中的效果名必须已声明；函数箭头、Record/能力字段及未使用的类型别名中的效果名都会检查。导入模块的标签使用 `{M.Audit}`，不同模块别名下的标签不同；`Console` 保持全局内建身份。当前沿用既有模块别名与重载规则，没有另行引入基于规范文件路径的标签身份。

箭头上的集合描述调用可能产生的潜在效果；表达式另有立即效果。取出函数或构造闭包不自动执行其 body；调用会将函数潜在效果和求值实参的立即效果传播出去。分支/模式分支采用保守合并，即使运行时某个分支未执行，其效果仍可能出现在静态集合中。

未写效果标签的箭头表示纯上界，例如 `String -> Unit` 不接受需要 Console 的函数。纯函数可以满足允许额外固定标签的上界。普通函数参数/返回值位置有相应方向检查，不能把效果包含当成无条件对称相等。

内部支持效果变量和高阶传播，例如 map 的回调效果可以传播到 map 的执行箭头。用户当前能在注解中写固定的大写标签集合；没有显式效果变量绑定/forall 语法。完整的开放集合约束求解能力仍有限，不把 R4 说成任意效果类型系统。

`putStr`、`putStrLn` 引入 Console，求值器记录输出事件，再由 Main 重放。`:t` 不执行这些操作；错误之前已经产生的事件可以保留。空效果集合只表示没有被追踪的效果，不保证终止、不保证不会除零或发生运行期错误。

效果标签没有自动对应的字典值。基础版必须先声明 `effect FileIO;` 才能写 `{FileIO}`，但这仍不会创建文件操作或 handler。主版本已采用此声明检查。

能力与效果分别描述隐式依赖和调用上界。例如 Writer 的 emit 方法可以携带 Console；调用该字段会传播 Console，构造 Writer 或取出字段本身不执行方法。纯 mock 可以满足带 Console 上界的方法类型，通过该接口调用仍保守保留 Console。自动搜索得到的字典必须纯构造，不能借此隐藏立即副作用。

主版本目前不支持 `{State Int}` 等参数化效果、显式效果变量语法、`perform` 或 handlers，也没有新增语言级可恢复异常、可变状态操作。内部高阶效果传播仍保留。

## 8. Prelude 与 Tuple

当前工作区的入口为 `show`，`Show` 字段名为 `show'`；`showInt` 已是 `Show Int` 字典，整数格式化函数为 `_showInt`。不要继续将旧文档中的 `showInt n` 当作当前接口。

当前还存在 Ord/Show 的列表、Maybe、Either 等实例，以及二元组 Show 实例。`showStr` 使用 `id`，字符串展示目前不会自动添加引号或转义；“展示字符串必须可重新解析”不是现有保证。

元组按固定元数区分类型，二元 `(a,b)` 与三元 `(a,b,c)` 不同。普通 HM 实例不能一次覆盖所有元数；二元嵌套 `(a,(b,c))` 可以递归组合二元实例，但不等同于三元元组。实例是否存在以当前 Prelude 为准。

Prelude 正在演进；其未提交改动属于用户。本轮文档维护不改写 Prelude。

## 9. 导入、诊断与当前缺口

文件导入写作 `import "path" as M;`，通常独占一行。别名必须大写起始，`Prelude` 保留。相对路径按当前加载文件目录解析，模块值/类型/构造器通过限定名使用。不要将它等同于拥有签名、私有成员或抽象类型的参数化模块系统。

已有源码位置、定义/调用说明、隐式缺候选/歧义、效果不匹配与求值错误。类型检查器有局部恢复机制，但当前 Main 遇到一个坏定义时可能拒绝整批提交；“能够继续检查”不等于“其余定义一定成功提交”。

当前没有同名字段多态、do、guard、模式函数方程、完整 row polymorphism、HKT、higher-rank 字段、GAT 或 handlers。Ability 与固定 effect 声明已采用到主源码；参数化 effect、handler 和 do 的新版隔离交付见第 10 节。

源码对应入口：[Parser](../src/Interp/Parser.hs)、[TypeCheck](../src/Interp/TypeCheck.hs)、[Eval](../src/Interp/Eval.hs)、[Types](../src/Interp/Types.hs)、[Main](../app/Main.hs)、[Prelude](../utils/prelude.txt)。历史测试证据见 [归档](../TODO/archive/README.md)，当前验收工作见 [路线](../TODO/32-当前路线与验收.md)。

## 10. 隔离交付：参数化 Effect、handler 与 do

本节对应 [36 的完整代码与验证记录](../TODO/36-参数化Effect与深层Handler完整代码.md)，需要使用隔离解释器或采用配套补丁。当前主版本的 `cabal run interp` 尚不支持本节扩展。

```text
effect State s { get :: Unit -> s, set :: s -> Unit };
effect Error e { raise :: e -> a };

handle Error String in do { raise "bad"; 99 } with {
    return x -> Right x;
    raise err resume -> Left err;
};
-- Left "bad"；99 不执行。

(handle State Int in do { x=get Unit; set (x+1); get Unit } with {
    return x -> lambda state -> (x,state);
    get u resume -> lambda state -> resume state state;
    set value resume -> lambda state -> resume Unit value;
}) 10;
-- (11,11)
```

声明操作的所属效果自动加入第一条调用箭头；`get` 的类型为 `Unit -> {State s} s`。操作是可传递的普通函数值，但调用发出请求。操作接收一个显式参数，多参数可用 Tuple；复杂效果类型参数加括号，例如 `{State (Int,String)}`。没有隐式操作参数或显式 forall 语法；操作声明自身的额外类型变量按每次调用实例化，在 handler 分支中按刚性变量检查。

Handler 显式指定目标效果及类型参数，并且必须有 return 分支、完整且不重复的操作分支。`resume` 是该分支绑定的普通局部名字，不是关键字；可以不用，但同一恢复值最多调用一次。深层恢复会重新进入同一个 handler，操作分支和 return 分支的代码则在它之外执行，因此可以向外层转发。最近的同名 handler 优先，没有具名 effect 实例；一个未处理效果行中的同名参数必须统一。

静态结果消除被处理计算的目标效果，保留其他效果、return/操作分支产生的效果，以及恢复后返回函数的潜在效果。类型显示中的 `{e0 \\ State}` 是内部残余效果记法，用户不能把它当作已经支持的源类型语法。

`do { x=f a; g x; h x }` 从左到右执行全部语句，返回最后表达式；未使用的绑定也初始化。支持遮蔽、嵌套和末尾分号；不支持空块、以绑定结尾或模式绑定。绑定名只在后续语句可见；纯 lambda 保持多态，执行效果的值初始化采用保守泛化限制。普通 let 仍保留既有字典延迟行为，不能把两者混同。

State/Error 的处理器由用户代码实现，示例与通用 `runState`、`runError`、显式回滚 `attempt` 都在 36 配套程序中。Error 本身不保证状态回滚，嵌套处理顺序决定返回状态是否被保留。

隔离版的宿主接口包括：

```text
putStr   : String -> {Console} Unit
putStrLn : String -> {Console} Unit
readFile : String -> {FileIO} Either String String
writeFile : (String,String) -> {FileIO} Either String Unit
```

Console 继续记录输出事件并按既有批次规则重放。FileIO 默认进行 UTF-8 文本整文件读写，路径相对解释器进程工作目录，失败返回 Left；调用发生时执行文件操作。用户 handler 可以拦截两者做 mock，宿主接口不允许被 effect 声明改写。需要把文件错误转换成 Error 时，用户显式匹配 Either 并调用 raise。

本次仍未加入多次恢复、异步、具名效果实例或资源区域类型。一次性检查由运行时令牌执行；会话保存使用记录，尚未设计令牌回收。更多原理、完整补丁、限制及复现命令见 36。
