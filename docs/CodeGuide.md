# 代码导览（CodeGuide）

> 目的：帮助你在手动改代码时，快速回忆「每个函数、每块实现到底在干什么」。
> 结构：Types/Parser/Pretty 只做**简表**（一句话/函数）；Eval、Computation、Builtin、Qualify、Synonym、Main 做**详解**；TypeCheck 全家做**最深讲解**（讲清具体怎么实现的）。
> 所有 `文件:行号` 均对应当前源码，改代码后行号会漂移，请以函数名为主锚点。

---

## 0. 全局流水线（先记住这条链）

一段源码从字符串到求值结果，依次经过：

```
源码文本
  │  Parser.hs            parse 出 (Tabs, [Statement])
  ▼
[Statement]  ── recProgram/desugarE   记录语法脱糖（rec/字段访问 → 构造子应用）
  │            Synonym.hs expandProgram 类型同义词展开
  │            Qualify.hs qualifyProgram 模块名限定（加 "Alias." 前缀）
  ▼
Program（规范化）
  │  TypeCheck.hs programChecker        类型检查 + 隐式字典 elaboration
  ▼
CProgram（带 elaborated body 的语句）
  │  Eval.hs evalCheckedProgram         求值
  ▼
V'（运行时值） + OutputEvent（Console 输出）
```

关键点：
- **类型检查和求值是两个独立阶段**，中间产物是 `CProgram`。TypeCheck 会把隐式参数「elaborate」成显式的 `DictCall`/`Lambda`，求值阶段看到的已经是脱糖后的代码。
- 三个 monad 贯穿始终：
  - `Checker m`（TypeCheck/Common.hs:16）= `MonadState Counter` + `MonadReader DEnv` + `MonadError (Located TypeError)`。
  - `Eval m`（Eval.hs:26）= `MonadState Runtime` + `MonadReader Env` + `MonadError (Located EvalError)`。
- REPL/文件驱动逻辑全在 `app/Main.hs`；核心库在 `src/Interp/`。

---

## 1. Types.hs —— 数据结构速查（简表）

`Types.hs` 只有数据类型和少量纯工具函数，没有复杂逻辑。按用途分组：

### 1.1 语法树（parser 产出）

| 类型 | 含义 |
|------|------|
| `E'` | 表达式。构造子：`ILit/BLit/SLit`（字面量）、`Var`、`ListLit`、`TupleLit`、`Let/StrictLet`（惰性/严格绑定）、`Handle`（效应处理器）、`DictCall`（字典调用，elaboration 产物）、`If`、`BOpr`（二元运算）、`Lambda`、`App`、`Match`、`AnnT`（类型标注 `e : T`）、`At Span`（附带源码位置）、`ImpHole`（隐式洞，待填）。 |
| `P'` | 模式。`PVar/PWild/PInt/PBool/PStr/PNil/PCtor/PCons/PTuple`。 |
| `T'` | 类型。`TInt/TBool/TString/TVar Int/TList/TTuple/TFunc a eff b/TCon n args`。注意函数类型是 `TFunc 参数 效应行 返回值`——效应挂在函数箭头上。 |
| `Decl` | `Def Text [Param] E'`：一个定义（名字、参数表、函数体）。 |
| `Param` | `ExplicitParam Text (Maybe T')`（显式参，可选类型标注 `(x :: T)`）/ `ImplicitParam Text T'`（隐式参 `{x : T}`）。 |
| `Statement` | 顶层语句：`StmtDef/StmtImp`（隐式定义）`/StmtExpr/StmtData/StmtAbility/StmtEff/StmtOperations/StmtType/StmtInfix`。 |
| `DataDecl` | `data`/`ability` 声明：名字、类型参数、构造子列表、字段表。 |
| `EffDecl` | 效应声明：名字、参数、操作列表 `[(操作名, 操作类型)]`。 |
| `Opr/LitOpr/CmpOpr` | 运算符。`BOpr Opr` 里的 `Opr` 分算术/比较/逻辑等。 |

### 1.2 效应行（R4 设计，核心中的核心）

| 字段 | 含义 |
|------|------|
| `Eff`（= `EffRow`） | `Eff { effLabs :: Set Text, effVars :: Set Int, effParams :: Map Text [[T']], effExclusions :: Map Int (Set Text) }`。 |
| `effLabs` | 确定的效应标签集合，如 `{"Console","FileIO"}`。 |
| `effVars` | 效应行变量（未定的行，用 `Int` 标识），泛化时保留。 |
| `effParams` | 参数化效应：效应名 → 它携带的类型参数（如 `State Int`）。 |
| `effExclusions` | 行变量 → 该变量**不能**等于哪些标签（求解时排除用）。 |
| `emptyEff` | `Eff S.empty S.empty`（Types.hs 内定义），纯净无效应。 |

### 1.3 类型方案与约束

| 类型 | 含义 |
|------|------|
| `S'`（= `Scheme`） | `Scheme { sVars（全称量化的类型变量）, sPreds :: [Pred]（隐式约束）, sType, sDictCall :: Eff（调用该函数需要的效应）, sEffCs :: [(Eff,Eff)]（效应约束）, sInitEff }`。 |
| `pattern Forall vs ps ty` | 便捷模式：只关心前三个字段时用；构造时后三个字段填 `emptyEff/[]/emptyEff`。`{-# COMPLETE Forall #-}` 让 case 用它也算穷尽。 |
| `Pred` | 隐式约束：`Implicit T'`（匿名）/`NameImplicit Text T'`（具名，可被按名搜索）/`AutoImplicit Text T'`。 |
| `predType/predName/predSearchName` | 从 `Pred` 取类型 / 取名字 / 取「可搜索名」（只有 `NameImplicit` 有）。 |
| `pureFunc a b` | `TFunc a emptyEff b`，纯函数类型的简写构造器。 |

### 1.4 检查器状态与错误

| 类型 | 含义 |
|------|------|
| `Counter` | 类型检查全局状态：`nextId`（发新变量用）、`effDefs :: Map Int Eff`（效应行变量的定义）、`effRels :: [(Eff,Eff)]`（效应等价关系）、`effTypeSub :: TSub`（效应内部的类型替换）、`effParamRows :: Set Int`（哪些行变量是「参数行」）。 |
| `TSub` | `Map TypeVar T'`，类型替换。 |
| `TypeError` | ~30 个构造子，覆盖所有类型错误：`UnboundVar/TypeMismatch/OccurCheck/NonExhaustivePat/CtorArityMisMatch/AmbiguousImp/EffMismatch/EscapedImpHole/...`。`CalleeNote/WithNote` 用来给错误附加上下文说明。 |
| `Located e` | 给错误附带 `Maybe Span`（源码位置），方便报错定位。 |
| `Wanted` | 一个「待解决的隐式约束」：包含 pred、洞 id、位置等，elaboration 期间逐步填。 |

### 1.5 运行时与编译产物

| 类型 | 含义 |
|------|------|
| `V'` | 运行时值：`VInt/VBool/VStr/VList/VTuple/VClosure/VCtor/VPrim`（宿主原语）`/VOperation`（效应操作）`/VResume`（续体）。 |
| `Runtime` | 求值状态：深度计数、resume token 集合 `rtResumeUsed`、输出事件序列等。 |
| `Computation` | 求值的中间态 monad-like：`Done/Failed/Request/Primitive/FreshResume/UseResume`。`Request` 表示「抛出一个效应操作，等 handler 接管」。 |
| `OutputEvent` | `WriteText/WriteLine`，Console 输出被记录成事件（便于测试断言）。 |
| `EvalError` | 运行时错误：`RecursionLimited/UnhandledEff/MatchError/...`。 |
| `Env/TEnv` | `Map Text V'`（值环境）/ `Map Text S'`（类型环境）。 |
| `IEnv` | 隐式环境 = `[IFrame]`（分帧的隐式候选表，作用域隔离）。 |
| `DEnv` | 声明环境：`denvDatas`（数据类型→构造子名）、`denvCtors`（构造子→`CtorInfo`）、`denvEffs`、`denvAbilities`、`denvOperations`（效应名→`EffDecl`）、`denvFields`（data→字段→(构造子,下标)，类型驱动字段解析用）。 |
| `FieldTab` | `Map Text [FieldInfo]`：字段名→所有拥有它的构造子信息（一名可属多个 data，故值是列表）。`FieldInfo{fiCtor,fiData,fiIdx,fiArity,fiSolo}`。仅用于解析期的记录**构造/模式**（按构造子名过滤）；字段**访问/更新**改由类型检查期解析。 |
| `CtorInfo` | 构造子信息：`ciArgs`（参数类型）、`ciRes`（结果类型）、`ciTvs`（该构造子的类型变量）。 |
| `StmtTy` | 语句类型检查结果：`TyDef/TyDefFailed/TyDefSkipped/TyExpr`。 |
| `InferRes` | 单表达式推断结果：类型、替换、待解约束、elaborated 表达式、效应等打包。 |
| `CBinding/CProgram` | 编译后的绑定 / 编译后的程序（TypeCheck 的最终产物，喂给 Eval）。 |
| `Session` | REPL 会话全局态：`sEnv/sTEnv/sImp/sDEnv/sFixities/sFields/sTrace/...`。 |
| `Tops` | 一个模块的顶层名字集合：`tVals/tTypes/tCtors`，Qualify 用它决定给哪些名字加前缀。 |
| `ImportDecl/ModuleRef/ModuleExports` | 模块系统：导入声明、模块引用、导出闭包。 |
| `SynTable` | `Map Text ([Text], T')`：类型同义词表（名→参数,展开体）。 |

### 1.6 工具纯函数
`Param` 的取名单元、`predType/predName` 等见上。`Counter` 的默认值、`emptyEff`、`pureFunc` 都在这里，供全项目复用。

---

## 2. Parser.hs —— 解析器（简表）

基于 `megaparsec`，`type Parser a = Parsec Void Text a`（Types.hs）。核心是一个**优先级/结合性驱动**的运算符解析器（`Tabs` = 运算符表）。

### 2.1 词法与命名
| 函数 | 作用 |
|------|------|
| `opInfo/reservedOps/isReservedOp/isOpChar/isOpName`（24-46） | 运算符的元信息与判定（哪些字符能构成运算符、保留运算符）。 |
| `builtinFix/builtinOp/defaultFix`（46-56） | 内建运算符的结合性/优先级；未知运算符用 `defaultFix`。 |
| `reserved/reservedType/isVarFirst/isVarLeft`（57-75） | 关键字表；变量名首字符/后续字符合法性判定。 |
| `lexeme/nextNotVar/symbol/toBool/comma/semicolon`（76-96） | 词法基元：吃掉尾随空白、符号匹配、逗号/分号。 |
| `nameRaw/qname/varName/upperName`（145-159） | 标识符解析：原始名、限定名（`A.b`）、变量名（小写开头）、构造子名（大写开头）。 |
| `varIndex`（97） | 把 `a0`、`a1` 这种类型变量名解析成 `Int`。 |

### 2.2 表达式解析（自顶向下）
| 函数 | 作用 |
|------|------|
| `parseExpr`（550） | 表达式入口。 |
| `parseOpr`（316） | **运算符优先级解析核心**：按 `Lev`（优先级层）递归下降，处理结合性。 |
| `parseApp`（347） | 函数应用（左结合，`f a b` = `App (App f a) b`）。 |
| `parseAtom/parseAtomHead/atomTail`（250-262） | 原子表达式 + 后缀（字段选择、记录更新）。 |
| `parseParen`（216） | 括号：区分单元组 `()`、tuple `(a,b)`、类型标注、分组。 |
| `parseUnary`（239） | 一元前缀（负号等）。 |
| `parseLambda`（339） | `lambda x -> body`（注意关键字是 `lambda`，不是 `\`）。 |
| `parseLet`（285） | `let`/`strict` 绑定。 |
| `parseIf`（310） | `if/then/else`。 |
| `parseAnn`（435） | `expr : Type` 类型标注。 |
| `parseDo`（471） | `do { ... }` 块：语句序列脱糖成 `>>=` 链，**必须以表达式结尾**。 |
| `parseHandle`（491） | `handle body with Eff { ret x -> ...; op x k -> ... }` 处理器语法。 |
| `parseMatch/parseArm`（815/820） | `match ... | pat -> ... | pat -> ...`（用 `|` 分隔臂）。 |
| `parseDictArgs`（358） | 显式字典参数 `{x = expr}`。 |
| `operatorExpr/operatorValue`（205/210） | 把运算符名转成 `BOpr` 或部分应用。 |

### 2.3 字面量、模式、类型
| 函数 | 作用 |
|------|------|
| `parseILit/parseBLit/parseListLit/parseStrLit/strLiteral/strChar/strEscape`（136-191） | 整型/布尔/列表/字符串字面量与转义。 |
| `parsePat*`（776-831） | 模式：`parsePat`入口，`parsePatAtom/parsePatArg/parsePatCtor/parsePatList/parsePatParen`等；`parsePatCtor`（831）= `PCtor n <$> many parsePatArg`。 |
| `parseCons`（823） | 列表 cons 模式 `x :: xs`。 |
| `parseType*`（371-434） | 类型：`parseType`入口，`parseTypeApp`（应用）、`parseTypeAtom/parseTypeName/parseTypeParen/parseTypeList/parseTypeWith`、`parseEffRow/parseEffRowWith`（效应行 `{Console, State s}`）。 |

### 2.4 声明与顶层
| 函数 | 作用 |
|------|------|
| `parseDef/parseImpDef/defName/opName`（567-589） | `def f x = ...` 与隐式 `imp` 定义。 |
| `parseData/parseAbility/parseDataCore/parseCtorDecl`（443-549） | `data`/`ability` 声明与构造子。 |
| `parseEff`（454） | `eff Name op1, op2`（逗号分隔操作）。 |
| `parseTypeDecl/parseInfixDecl/assocOf`（590-620） | `type` 同义词声明 / `infix` 结合性声明。 |
| `parseParam/parseImpParam`（160-167） | 显式/隐式参数。 |
| `parseImportLine/parseImportLineText`（102-135） | `import "path" as M`。 |

### 2.5 记录脱糖（parse 之后立即做）
| 函数 | 作用 |
|------|------|
| `recProgram`（840） | 遍历程序，收集字段表 `FieldTab`（`addField` 现允许跨 data 同名，一名多格）并脱糖记录**构造/模式**。 |
| `recStmt/desugarE/desugarP`（882/892/1015） | 语句/表达式/模式里的记录脱糖。`desugarE` 的 `App` 分支现在**只**脱糖记录构造 `$rec@`；字段访问 `$fld@`、更新 `$upd@` 保留为 marker，交给类型检查期解析（见 §10.8 `inferFieldAccess`）。 |
| `recBuild`（956） | `Ctor{ f = v }` 构造翻译成位置构造子应用（按构造子名过滤字段，无歧义）。 |
| `ctorFields/zipFields`（950/993） | 按构造子名取字段列表（`ctorFields` 遍历列表值表过滤 `fiCtor`）、构造参数配对。 |
| `spanOf/badSpan`（921/1010） | 位置信息与报错辅助。（原 `fieldGet/recUpdate/lookupField/recUse/isMarker/oldTmp` 已随字段解析迁移到 Infer 而删除。） |
| `parseAnnParam`（163） | 带类型标注的显式参数 `(x :: T)` → `ExplicitParam x (Just T)`。 |
| `dataDeclsOf`（874） | 从程序里抽出所有 `DataDecl`（Qualify/Main 也用它）。 |

---

## 3. Pretty.hs —— 打印/报错（简表）

| 函数 | 作用 |
|------|------|
| `printT'/prettyT'`（82/104） | 打印类型；`printT'` 会先 `normalizeT` 把类型变量重命名成 `a0,a1,...`。 |
| `prettyEff`（381） | 打印效应行 `{Console, State s}`。 |
| `prettyS'`（171） | 打印类型方案（含 `forall`、约束、效应）。 |
| `prettyP`（216） | 打印模式。 |
| `prettyBOpr`（17） | 打印运算符。 |
| `debugE'`（20） | 打印表达式 AST（调试用）。 |
| `prettyV'`（43） | 打印运行时值。 |
| `prettyTypeError`（107） | 把 `TypeError` 变成人类可读文本（每种错误一句话）。 |
| `prettyEvalError`（61） | 同上，运行时错误。 |
| `renderLocated`（241） | 把带 `Span` 的错误渲染成「文件:行:列 + 源码行 + `^` 指示」。 |
| `prettyTypeErrorWith/prettyEvalErrorWith`（231/228） | 带 batch 名的错误渲染（REPL 里区分哪一批）。 |
| `printBatch`（261） | REPL 打印一整批语句的类型/结果/错误。 |
| `renumberT/varOrder/normalizeT/normalizeTs`（307-340） | 类型变量规范化重命名（按出现顺序映射到 `a0..`）。 |
| `lineAt/errLine/spanStart`（236-408） | 从源码取第 N 行、错误的行号、span 起点。 |
| `fixityTxt/quoted/escape/hex4`（345-380） | 结合性文本、字符串加引号与转义（含 `\uXXXX`）。 |
| `fieldNames`（210） | 打印记录字段名。 |
| `dedup`（341）/`effVarOrder`（392）/`tick`（408） | 去重 / 效应变量排序 / 给名字加 tick 标记。 |

---

## 4. Builtin.hs —— 内建环境（详解）

宿主提供的「原语」都集中在这里，是**唯一的真值源**（之前 Console/FileIO 签名在多处重复，已合并到此处）。

| 函数 | 详解 |
|------|------|
| `builtinEnv`（10） | `Env`：把原语名映射到 `V'`。`putStr/putStrLn` 是 `VPrim`（宿主直接执行的函数）；`readFile/writeFile` 是 `VOperation "FileIO" ...`（效应操作，运行时会抛 `Request`，需要 handler 或宿主接管）。 |
| `builtinTEnv`（25） | `TEnv`：上述原语的类型方案集合（`outputTy` 等）。 |
| `outputTy`（40） | `putStr/putStrLn` 的类型：`String -> {Console} Unit`（`S'`）。 |
| `fieldErrTy/consTy/strAppTy/unconsTy/fromCodeTy/intCompareTy`（43-60） | 其它原语的类型方案：列表 cons/uncons、字符串拼接 `append`、字符码转换、整数比较等。 |
| `readFileTy`（61） | `readFile : String -> {FileIO} (Either String String)`。**权威签名**——读文件成功返回 `Right 内容(String)`，失败返回 `Left 错误信息(String)`。 |
| `writeFileTy`（64） | `writeFile : (String, String) -> {FileIO} (Either String Unit)`。参数是 (路径, 内容)。 |
| `putStrOpTy/readFileOpTy/writeFileOpTy`（102-109） | 上面几个 `S'` 的底层 `T'` 片段，抽出来复用，避免重复拼类型。 |
| `consoleEffDecl`（111） | `EffDecl "Console" [] [("putStr", putStrOpTy), ("putStrLn", putStrOpTy)]`——Console 效应的操作声明。 |
| `fileIOEffDecl`（114） | `EffDecl "FileIO" [] [("readFile", readFileOpTy), ("writeFile", writeFileOpTy)]`。 |
| `consoleEff`（90）/`fileEff`（93） | `Eff (S.singleton "Console") S.empty` / FileIO 版——单标签效应行。 |
| `initSession`（67） | REPL 初始 `Session`（填入 builtinEnv/TEnv、空 DEnv 等）。 |
| `emptyDEnv`（84） | `DEnv M.empty M.empty S.empty (S.fromList ["Console","FileIO"]) M.empty`。**注意 `denvOperations` 是空的**——所以 `inferHandler` 处理宿主效应时永远走「找不到就用内建 fallback」的分支（见 Infer.hs:346）。`denvEffs` 含 Console 和 FileIO。 |
| `emptyCounter`（87） | 给一个起始 id 造空 `Counter`。 |

> 改这里要小心：`readFile/writeFile` 的**类型**（此处）、**运行时实现**（Main.hs 的 host 函数）、**语言文档**（LanguageReference）三处必须一致。历史上 FileIO handler bug 就是因为 Infer.hs 里另写了一份错的 fallback 签名。

---

## 5. Computation.hs —— 效应计算的 monad（详解，47 行）

`Computation` 是求值的核心中间态。它不是标准 monad，而是手写的「可能抛效应请求」的计算树。`handleC` 实现**深处理器**语义。

| 函数 | 详解 |
|------|------|
| `bindC`（11） | `Computation` 的 `>>=`。把上一步的结果喂给下一步函数；若上一步是 `Done v` 就继续，若是 `Failed/Request` 等则原样传播（短路）。这决定了「效应请求如何冒泡」。 |
| `mapFailure`（20） | 给 `Failed` 分支附加 `Span`（源码位置），用于错误定位。递归下钻，保留结构。 |
| `handleC`（33） | **深处理器核心**。签名：`handleC mkResume envMap ... effName opKey ... arms computation`。逻辑： |

`handleC` 的具体实现（务必理解，这是代数效应的关键）：
1. **`Done v`**：body 正常结束 → 走 `return` 分支（handler 的 `ret`）。
2. **`Request`**（body 抛出效应操作）：
   - 若请求的效应名 == 目标效应名，且能在 `arms` 里找到匹配的操作臂：
     → 构造一个 **`FreshResume`**，其中 `VResume` 是一个「重新进入 handler 循环」的续体（resume-once 语义：调 `k` 会用剩余计算继续，但只能调一次，靠 `rtResumeUsed` token 保证）。
     → 执行对应臂 `(o, x, k, e)`：把操作参数绑到 `x`、续体绑到 `k`，求值 `e`。
   - 若 `arms` 里没有覆盖该操作 → 抛 `UnhandledEff`（handler 必须覆盖每个操作恰好一次）。
   - 若请求的是**别的**效应（非目标）→ 该 `Request` 被**透传**：包在一个 loop 里继续向外冒泡，让外层 handler 处理。这就是「效应转发/嵌套 handler」的机制。

> 深处理器 = resume 后**重新进入 handler**，所以 handler 内状态能持续。这跟浅处理器不同。相关测试见 Spec.hs「参数化 effect、handler 与 do」。

---

## 6. Eval.hs —— 求值器（详解）

`Eval m` 约束（26）= `MonadState Runtime` + `MonadReader Env` + `MonadError (Located EvalError)`。

### 6.1 入口与深度控制
| 函数 | 详解 |
|------|------|
| `evalwDepth`（28） | 带深度守卫的求值。取当前 `rtDepth`，超过 `maxDepth`(=10000, 217) 抛 `RecursionLimited`。求值前后 `restoreDepth` 复位，用 `catchError` 保证出错也复位（避免深度泄漏到兄弟分支）。 |
| `eval`（39） | `ask >>= \env -> gets rtDepth >>= \d -> runC (evaluate env d e)`：拿环境和深度，跑 `evaluate` 得到 `Computation`，再 `runC` 驱动它。 |
| `runC`（42）/`runCWith`（48） | 驱动 `Computation` 到 `V'`。`runC` 用 `defaultHost`；`runCWith` 接受自定义 host（处理 FileIO 等真实 IO）。遇到 `Request` 时调 host。 |
| `defaultHost`（45） | 只处理 Console（putStr/putStrLn 记录成 `OutputEvent`）；其它效应请求报错。 |
| `evalWith`（73） | `runCWith` + `evaluate` 的组合入口。 |
| `resumeNext`（67） | 分配一个新的 resume token id（配合 `rtResumeUsed`）。 |

### 6.2 表达式求值核心
| 函数 | 详解 |
|------|------|
| `evaluate`（76） | `Env -> Int -> E' -> Computation`。**大 case**，每种 `E'` 一个分支。重点： |

`evaluate` 的关键分支实现：
- **`Let n (Lambda ...) body`**：把闭包**打结**（knot-tying）绑进环境，使函数体能引用自己 → 递归函数。
- **`StrictLet`**：总是先求值 RHS 到 WHNF 再绑定（严格）。
- **`Var`**：查环境，未找到报 `UnboundVar`（运行时版）。
- **`VOperation`（效应操作被应用）**：产生 `Request`——把操作抛给最近的 handler。
- **`putStr/putStrLn` 硬编码**：直接产生 Console `Request`（这是有意设计，不是 bug；见 DN 记录）。
- **`App`**：求函数、求参数、`applyValue`；`Match` 走 `matchArms`；`Handle` 构造 handler；`DictCall` 应用字典。

### 6.3 程序级求值（多语句 + SCC）
| 函数 | 详解 |
|------|------|
| `evalProgram`（137） | 未检查程序的求值（旧路径）。 |
| `evalCheckedProgram`（140）/`...With`（145） | 求值 `CProgram`（TypeCheck 产物）。`With` 版接受自定义 host。 |
| `evalProgramSkipping`（149）/`...With`（152） | 跳过指定 id 集合的语句（REPL 里避免重复求值已定义的）。 |

程序级求值的实现要点（152 起）：
- 逐语句求值，用 `tryEnv`（253）把每条的错误 catch 成 `Left`，**不中断整批**（一条错，后面照跑）。
- 用 **SCC（强连通分量）** 组织定义顺序（`deps`/`flatten`, 242/248）：互相递归的定义放同一 SCC。
- **零元（nullary）非 lambda 定义被 EAGER 求值**（在绑定时立即算）。这是为什么「在 handler 外面写 `def prog = do{...}`」会得 `UnhandledEff`——它在绑定那一刻就求值了，此时不在 handler 作用域内。给 `prog` 加个参数即可避开。

### 6.4 运算与工具
| 函数 | 详解 |
|------|------|
| `calArithOpr`（190）/`calCmpOpr`（198） | 算术/比较运算符到具体函数的映射。 |
| `addSpan`（207）/`stripAt`（211） | 给 `Located` 加 span / 剥掉表达式外层的 `At`。 |
| `freeVars`（220） | 表达式的自由变量集合（SCC 依赖分析用）。 |
| `deps`（242） | 从 `Program` 算出 `(Decl, 名字, 依赖名列表)`，喂给 `stronglyConnComp`。 |
| `flatten`（248） | 把 `SCC Decl` 摊平成 `[Decl]`。 |
| `matchP`（267） | 单条模式匹配：`(P', V') -> Maybe [(名字, 值)]`，成功返回绑定。 |
| `matchArms`（320） | 依次试每个臂，第一个 `matchP` 成功的用它；全失败报 `MatchError`。 |
| `patVars/patVarsList`（256/259） | 模式里的变量集合/列表。 |
| `applyPrim`（287） | 应用宿主原语（`VPrim`）：putStr/append/cons/字符码等的真实实现。 |
| `recordOutput`（311） | 把一个 `OutputEvent` 追加进 `Runtime` 的输出序列。 |
| `eqV/eqOp`（327/338） | 值的结构相等（`eqV` 返回 `Maybe Bool`，不同类型不可比时 `Nothing`）。 |
| `paramName/explicitParams/implicitParams`（343-352） | 从 `[Param]` 取名字/显式参名/隐式参 (名,类型)。 |
| `declBody`（354）/`defsOf`（357） | 取 `Decl` 的函数体 / 从语句抽出所有 `Def`。 |
| `maxDepth`（217） | 递归深度上限 10000。 |

---

## 7. Qualify.hs —— 模块名限定（详解，116 行）

作用：把**模块内部**的顶层名字加上 `"Alias."` 前缀，实现 V1 模块作用域隔离。导入 `import "x" as M` 后，`M` 里的 `foo` 变成 `M.foo`，避免和主程序冲突。

| 函数 | 详解 |
|------|------|
| `topsOf`（15） | 扫描 `Program`，收集本模块定义的顶层名字：`tVals`（`def`/`imp` 定义的名字 + operations 名，排除运算符名）、`tTypes`（`data`/`type` 名）、`tCtors`（构造子名）。返回 `Tops`。 |
| `qualifyProgram`（23） | 主函数：对每条语句 `map (stmt tops)`。内部： |

`qualifyProgram` 的实现细节：
- 局部 `effs`（效应名集合，来自 `StmtEff` 和 `StmtOperations`）——**`Console`/`FileIO` 永不加前缀**（它们是宿主全局效应）。
- `qual s n`：若 `n ∈ s` 则返回 `a <> "." <> n`，否则原样。
- `stmt`：逐语句类型分派，对每种语句递归限定其内部的名字。`StmtDef/StmtImp` → `decl`；`StmtData/StmtAbility` → `dataDecl`（限定类型名、构造子名、字段里的构造子引用）；`StmtOperations` 限定操作名和类型；`StmtType` 限定类型名。
- `decl`（限定 `Def` 名、参数类型、函数体）；`expr`（递归限定表达式里的 `Var`——但绑定变量 `bs` 集合内的不加前缀，`Let/Lambda/Match` 会把新绑定变量插入 `bs`）；`pat`（限定构造子模式名）；`typ`（限定类型里的 `TCon` 名和效应行的标签/参数）。
- `operationNames n`：从程序里找效应 `n` 的操作名集合，供 `Handle` 臂限定用。

> 直觉：`qualifyProgram` = 「给这个模块的所有自由顶层名字打上模块前缀」，绑定变量和本地变量不动，宿主效应不动。

---

## 8. Synonym.hs —— 类型同义词展开（详解，112 行）

作用：把 `type Foo a = ...` 定义的同义词，在类型检查前**全部展开**成底层类型。带环检测。

| 函数 | 详解 |
|------|------|
| `expandProgram`（14） | 入口：先 `foldM addSyn` 收集所有 `StmtType` 到 `SynTable`，再 `mapM expandStmt` 展开每条语句。返回新的 `(SynTable, Program)`。 |
| `expandStmt`（25） | 逐语句类型展开：`StmtDef/StmtImp` 展开 `Decl`；`StmtExpr` 展开表达式；`StmtData/StmtAbility` 展开构造子参数类型；`StmtType` 展开右边类型；`StmtOperations` 展开操作类型；其余（`StmtEff/StmtInfix`）原样。 |
| `expandDecl`（43） | 展开 `Def n ps body`：参数里的 `ImplicitParam` 类型 + 函数体表达式。 |
| `expandE`（50） | 递归展开表达式里所有子表达式和标注类型（`AnnT`）。`ImpHole` 原样。 |
| `expandT`（74） | **核心**：`go seen t` 带「已展开集合」`seen` 做环检测。遇 `TCon n args`： |

`expandT` 的实现（74 起）：
- `TCon n args`，查 `SynTable`：
  - 不在表里 → 普通类型构造子，只递归展开 `args`。
  - 在表里（是同义词）：
    - `n ∈ seen` → 报 `cyclic type synonym`（自己展开成自己）。
    - 参数个数不匹配 → 报 arity 错误。
    - 否则：先展开实参 `args`，把形参 `[0..]` 映射到实参，`subst` 代入同义词体，再对结果 `go (insert n seen)`（继续展开，可能嵌套同义词）。
- 其它类型（`TInt/TVar/TList/TTuple/TFunc`）递归展开子类型；`TFunc` 还要展开效应行里的 `effParams`。

| `subst`（100） | 把 `Map TypeVar T'` 替换应用到类型上（`TVar i` 查表替换，其它递归）。注意也替换 `effParams`。 |
| `tshow`（111） | `T.pack . show`，报错文本拼接用。 |

> 展开发生在类型检查**之前**，所以 TypeCheck 看到的类型里已经没有同义词了（都是 `TCon` 底层形式）。

---

## 9. app/Main.hs —— 驱动层 / REPL / 模块系统（详解）

这是可执行程序的入口，把库函数串成 REPL 和文件执行，并实现模块导入导出。

### 9.1 入口与主循环
| 函数 | 详解 |
|------|------|
| `main`（39） | 解析命令行：有文件参数就 `runFile`，否则进 `loop`（REPL）。 |
| `loop`（51） | REPL 主循环：读一行，分派命令（`:t/:load/:q/:implicits/:trace/:modules/:reload/:unimport/import`）。 |
| `runFile`（77） | 加载并执行一个文件。 |
| `help`（629） | 打印命令帮助。 |

### 9.2 批处理执行管线（最重要）
| 函数 | 详解 |
|------|------|
| `execBatch`（83）/`execBatch'`（89） | **一批语句的完整执行管线**：`expandProgram`（同义词）→ `programChecker`（类型检查，传入**仅上一批**的 DEnv）→ `evalCheckedProgramWith`（求值）。`execBatch'` 的 host 函数处理 FileIO：`readFile/writeFile` 用真实 IO，结果包成 `VCtor Left/Right`。Console 输出被 `recordOutput` 记进 trace。 |
| `doTypeOf`（150） | `:t` 命令：只类型检查一个表达式，打印其类型方案。 |

### 9.3 加载脚本 / 模块
| 函数 | 详解 |
|------|------|
| `loadFile`（182）/`loadScript`（185） | 加载一个脚本文件进 session。`loadScript` 分批 `execBatch`。 |
| `loadModule`（227） | **隔离编译**一个模块：独立环境编译，带循环导入检测（`[FilePath]` 是导入栈）。返回 `ModuleRef`。 |
| `moduleExports`（278） | 计算模块的**导出闭包**：哪些类型/构造子/值应该对外可见（用 `closeTypes`/`ctorClosure` 递归收集被引用到的名字）。 |
| `restrictSession`（332） | 把 session 限制到只保留导出的名字（作用域隔离）。 |
| `applyView`（355） | 把模块视图合并进当前 session，做冲突检查。 |
| `checkImportTarget`（418） | 校验导入目标合法性。 |
| `mergeCounter`（431）/`mergeResume`（441） | 合并两个 `Counter`（取 nextId 较大者、并 effDefs/effRels）/ 合并 resume token 状态。 |
| `unloadModule`（445）/`deleteExports`（453） | `:unimport`：从 session 删掉模块导出的名字。 |
| `doImport`（472） | `import "path" as M` 的实现：`loadModule` + `applyView`。 |
| `reloadAll`（498） | `:reload`：重新加载所有已导入模块。 |
| `showModules`（512） | `:modules`：列出已导入模块。 |

### 9.4 导出闭包辅助（纯函数）
| 函数 | 详解 |
|------|------|
| `typeRefs`（524） | 类型里引用到的所有类型名集合。 |
| `typeLabels`（532） | 类型里出现的效应标签集合。 |
| `effTypeRefs`（540） | 效应行的 `effParams` 里引用的类型名。 |
| `schemeRefs`（543） | 一个 `Scheme` 引用到的 (类型名, 标签) 全集（含 sType/sPreds/各效应字段）。 |
| `closeTypes`（554） | 从一组类型名出发，沿 `data` 构造子和同义词**求闭包**（不动点 `go`），得到所有间接依赖的类型名。 |
| `ctorClosure`（569） | 类型名集合 → 其所有构造子名集合。 |
| `firstDup`（573） | 找列表里第一个重复元素（冲突检测用）。 |

### 9.5 路径与 IO 工具
| 函数 | 详解 |
|------|------|
| `slash`（579）/`canonKey`（582）/`canonicalPath`（585） | 反斜杠转正斜杠 / 平台规范化 key（Windows 小写化）/ 规范化路径。 |
| `resolveImportPath`（590） | 相对导入路径解析成绝对路径。 |
| `readFileSafe`（593） | 安全读文件，失败返回 `Left 错误文本`。 |
| `replayOutput`（598） | 把记录的 `OutputEvent` 重放到 stdout。 |
| `showTrace`（605）/`showImplicits`（613） | `:trace`/`:implicits` 命令实现。 |
| `dirOf`（693） | 取路径的目录部分。 |

### 9.6 Prelude 处理
| 函数 | 详解 |
|------|------|
| `preludePath`（644） | `"utils/prelude.txt"`。 |
| `withPrelude`（647） | 启动时加载 Prelude（定义 `Unit/Either/append` 等基础类型）。读不到就警告并继续。 |
| `addPreludePrefix`（678） | 给 Prelude 里的定义名加 `"Prelude."` 前缀（同时保留原名，用 `M.union`）。 |
| `aliasFields`（663） | 给模块别名下的字段/构造子名加前缀。 |
| `importLines`（683）/`blanckImportLines`（686） | 抽取 import 行 / 把 import 行替换成空行（Prelude 单独编译时不含 import）。 |

---

## 10. TypeCheck 全家 —— 最深讲解

这是重点。分五个模块 + 一个总入口。按**调用顺序**组织，讲清每块具体怎么实现。

### 10.0 心智模型（先建立框架）

- **HM 类型推断 + 效应行 + 隐式字典 elaboration** 三合一。
- 状态 `Counter` 里有：类型变量计数器、**效应行的等式约束**（`effRels`）、效应行变量定义（`effDefs`）、效应内部类型替换（`effTypeSub`）。
- 隐式参数不是运行时特性，而是**类型检查期 elaborate 成显式字典传递**：`f {x} = ...` 会变成 `Lambda $x -> ...` + 调用处插 `DictCall` 或 `ImpHole`（洞，事后 `resolveWanted` 填）。
- 一次「绑定组」检查 = 推断所有定义体 → 收集 `Wanted`（隐式约束）→ 解约束 → 填洞 → 泛化成 `Scheme`。

### 10.1 TypeCheck.hs —— 编排层

| 函数 | 详解 |
|------|------|
| `programChecker`（57） | **总入口**。`withInferRollback`（出错回滚 Counter）包住：先做重复检查（data/eff/operation/def 名字不能重复），用 `buildProgramDEnv p` 建本程序的 DEnv，`mergeDEnv` 合并进已有 DEnv，再 `mergeDEnv (buildFieldAbilities usedFields)` 注入合成的字段 ability（字段多态用），`local (const denv)` 把它设为 Reader 环境，对 `StmtType` 跑 `checkEffTypes`，然后调 `programCheckerCore`。 |
| `programCheckerCore`（84） | **真正的编排器**，最复杂的函数。流程见下。 |
| `sccChecker`（39） | 旧版：SCC 分组检查一组 `Binding`，返回 `(TSub, TEnv)`。 |
| `sccCheckerElab`（179） | 新版：SCC 分组 + **elaboration** + **错误恢复**。返回 `(TSub, [CBinding], Map id StmtTy, 失败名集合)`。 |
| `candidateHeadsOverlap`（210） | 判断两个隐式候选的类型「头部」是否重叠（`:implicits` 用来提示潜在歧义）。 |

**`programCheckerCore`（84）的实现步骤**（务必按此顺序理解）：
1. **建三个环境**：`ctorEnv`（构造子的类型方案，`ctorSchemes`）、`oprEnv`（操作方案）、`outer`（外部已有的 TEnv）。
2. **两趟隐式解析**（这是精髓）：
   - 第一趟：给所有声明算**临时类型** `provT`（provisional），`sccCheckerElab ordinaryDecls` 检查普通定义。
   - 第二趟：`sccCheckerElab implicitDecls`，对每个隐式定义 `validateCandidate`（校验它是合法候选，见 Implicit.hs）。
   - 第三趟：用填好的隐式类型 `impT`，**再检查一遍普通定义** `sccCheckerElab ordinaryDecls`（这次隐式候选已知，洞能解析）。
   > 为什么要多趟？因为隐式定义的类型可能依赖其它隐式定义，普通定义又依赖隐式定义——需要不动点式的迭代。
3. **逐表达式终检**：对每条语句的表达式跑 `inferExpr` + `resolveWanted`（解剩余隐式约束）+ `fillHoles`（把洞替换成实际字典）+ `closeBinding`（泛化）。
4. **重建/elaborate 语句**：把 elaborated body 代回语句，替换原始 body。
5. **组装 `CProgram`** 返回。

**`sccCheckerElab`（179）的实现**：
- 用 `stronglyConnComp(deps)` 把声明按依赖分 SCC（互相递归的进同一个 SCC，一起检查）。
- 对每个 SCC 尝试检查；**若失败**：给第一个坏定义标 `TyDefFailed`，给它的所有依赖者标 `TyDefSkipped`（级联跳过），继续检查后面的 SCC（错误恢复，一次报一批而非一个）。
- 返回累积的 `(TSub, [CBinding], 语句类型 map, 失败名集合)`。

### 10.2 TypeCheck/Common.hs —— 共享工具（81 行）

| 函数 | 详解 |
|------|------|
| `Checker m`（16） | 类型检查 monad 约束 = `MonadState Counter` + `MonadReader DEnv` + `MonadError (Located TypeError)`。 |
| `fstDuplicate`（18） | 找列表第一个重复（重复定义检测）。 |
| `withNote`（23） | 给一段检查动作的错误**附加说明**：catch 错误后包成 `WithNote text err` 再抛。 |
| `spinHead`（26） | 从表达式提取「调用的头名字」（如 `f a b` → `Just "f"`），用于错误提示「在调用 f 时出错」。 |
| `calleeNote`（34） | 若错误发生在某个已知函数的调用处，附加 `CalleeNote 名 scheme err`（把被调函数的类型也打出来，帮用户定位）。 |
| `swallowedArmsNote`（47） | 一段固定提示文本（match 臂被吞掉时的说明）。 |
| `bodySpan`（54）/`declName`（59） | 取函数体 span / 定义名。 |
| `unionEff`（62） | **合并两个效应行**：并 `effLabs`、并 `effVars`、合并 `effParams`、**交** `effExclusions`（排除集取交，因为要同时满足两边）。 |
| `removeEff`（72） | **从效应行删掉一个标签**：从 `effLabs` 删掉，并把它加进每个行变量的 `effExclusions`（该行变量今后不能等于这个标签）。这是 handler「消除效应」的实现——body 有 `{E}`，handler 处理后结果去掉 `E`。 |
| `reject`（81） | 抛一个纯文本错误（`Located Nothing`）。 |

### 10.3 TypeCheck/Declarations.hs —— 声明环境（122 行）

| 函数 | 详解 |
|------|------|
| `typeCtorArity`（18） | 查一个类型构造子的元数（`DEnv` → `Maybe Int`）。 |
| `checkTypeCons`（27） | 校验类型里所有 `TCon` 都已知且元数对，否则报 `UnknownTypeCtor`/`TypeCtorArityMisMatch`。 |
| `checkAbilityType`（44） | 校验一个类型是合法的 ability（隐式候选必须基于 ability）。 |
| `checkEffRow`（51） | 校验效应行里的标签都是已知效应。 |
| `checkEffTypes`（60） | 校验 `StmtType` 同义词右边类型的合法性。 |
| `buildDEnv`（68） | 从 `[DataDecl]` 建**只含 data** 的 DEnv（填 `denvDatas`、`denvCtors`、`denvFields`）。 |
| `buildProgramDenv`（77） | 从整个 `Program` 建 DEnv：在 data 基础上加 `ability`、`eff`、`operations`。**注意此处 `denvEffs` 含 "Console" 但不含 "FileIO"**（与 `emptyDEnv` 不同——后者两个都含；这是一个历史不一致点，改代码时留意）。 |
| `mergeDEnv`（84） | 合并新旧 DEnv：处理重定义、清掉过时构造子；`denvFields` 用 `M.union`（新覆盖旧）。 |
| `ctorInfo`（96） | 从一个 `DataDecl` 和参数类型造 `CtorInfo`（`ciArgs/ciRes/ciTvs`）。 |
| `ctorSchemes`（103） | 把一个 `DataDecl` 的所有构造子变成 `TEnv`（每个构造子一个 `Scheme`：`a -> ... -> TCon n [...]`）。 |
| `checkDataDecls`（108） | 校验一组 data 声明（构造子名不重复等）。 |
| `getterAbility/setterAbility`（127） | 字段名 → 合成 ability 名：`$get_x` / `$set_x`（字段多态的约束类型头）。 |
| `getterFieldOf/setterFieldOf`（133） | 反解：从 `$get_x`/`$set_x` 取回字段名 x。 |
| `buildFieldAbilities`（146） | 为一组字段名合成 getter/setter ability 的 DEnv 片段（`$get_x r a = $get_x {fldGet :: r->a}`、`$set_x r a = $set_x {fldSet :: a->r->r}`），并登记进 `denvAbilities`。由 `programChecker` 在建 denv 后 merge 进来，只对真实字段名合成（跳过 `$`-前缀防跨批增殖）。 |

### 10.4 TypeCheck/Pattern.hs —— 模式类型推断（127 行）

| 函数 | 详解 |
|------|------|
| `patType`（22） | **核心**：`P' -> m (TSub, T', TEnv)`。给模式推断类型，返回（替换、被匹配值的类型、模式绑定的变量环境）。各模式：`PVar/PWild` → 新类型变量；`PInt/PBool/PStr` → 对应基类型；`PTuple` → 递归各分量组 `TTuple`；`PCons h t` → 递归 head/tail，**unify** head 类型和 tail 的元素类型（`[a]` 里 head 是 `a`、tail 是 `[a]`）；`PCtor n ps` → 查 `denvCtors` 拿 `CtorInfo`，**freshen** `ciTvs`（造新的类型变量实例），arity 检查，递归各参数模式并 unify 到构造子参数类型，结果类型是 `ciRes`。 |
| `checkDupPatVar`（75） | 检查同一模式里变量名不重复（`(x, x)` 非法）。 |
| `irrefutable`（80） | 判断模式是否「不可反驳」（一定能匹配，如单个变量/通配）——用于 `Let` vs 需要 `Match` 的判定。 |
| `exhaustive`（89） | **穷尽性检查**：`DEnv -> T' -> [P'] -> Bool`。判断一组模式是否覆盖被匹配类型的所有情况。递归处理：`TInt/TString/TVar/TFunc`（无限域，需通配）、`TBool`（True/False 都要）、`TList`（`[]` 和 `::` 都要）、`TTuple`（各分量笛卡尔积穷尽）、`TCon`（每个构造子都要覆盖，且各自参数递归穷尽）。`match` 不穷尽报 `NonExhaustivePat`。 |

### 10.5 TypeCheck/Implicit.hs —— 隐式字典解析（146 行）

| 函数 | 详解 |
|------|------|
| `isClosedType`（24） | 类型是否「闭合」（无自由类型变量）——只有闭合类型的候选才好解析。 |
| `deferWanted`（27） | 判断一个 `Wanted` 是否应**延迟**（还不能解析，等更多类型确定）。 |
| `wantedPred`（32） | 从 `Wanted` 取出它的 `Pred`。 |
| `nameGroups`（39） | 给「匿名」的 wanted 槽自动起名（如从类型头部推 `ord0`：取最后一个 `.` 段小写 + 索引）。让洞有可搜索的名字。 |
| `resolveWanted`（55） | **递归字典解析核心**：`TEnv -> Wanted -> m E'`，返回填进洞的字典表达式。实现：在 `IEnv` 各帧里 `search`（按类型匹配）或 `byName`（按名匹配）候选；带**环检测**和**深度 64** 守卫（防无限递归）；找到候选后可能递归解析候选自身的约束。 |
| `groupWanted`（103） | 把 wanted 按 pred 分组（同类型的一起解析）。 |
| `validateCandidate`（112） | **校验一个隐式定义是合法候选**（R5 规则）：基于 ability、上下文变量是头部的子集、不自我要求、无即时效应、类型里无开放的效应行变量。不合法报 `InvalidImpCandidate`。 |
| `matchCandidateType`（129） | 候选类型和 wanted 类型的匹配：`Set TypeVar -> T' -> T' -> Maybe TSub`，成功返回替换。 |

### 10.6 TypeCheck/Elaboration.hs —— 洞填充与改写（184 行）

| 函数 | 详解 |
|------|------|
| `pureInfer`（24） | 造一个「纯」的 `InferRes`（给定表达式和类型，无约束无效应）。 |
| `underSpan`（27） | 给 `Wanted` 附加 span。 |
| `mapInferExpr`（32） | 对 `InferRes` 里的 elaborated 表达式应用一个改写函数。 |
| `freshInternal`（35） | 造一个内部名字 `$prefix@N`（elaboration 产生的隐藏参数名，用户写不出来）。 |
| `replaceSolvedHoles`（38） | 把已解出的洞（`HoleSol`）替换进表达式。 |
| `fillHoles`（59） | **填洞**：`HoleSol -> E' -> Either HoleId E'`。全部洞都填上返回 `Right 表达式`；有洞没填返回 `Left 洞id`（→ `EscapedImpHole` 错误）。 |
| `prepareDecl`（85） | 预处理 `Decl`：给隐式参数和标注里的**负位类型变量** freshen（避免和外部串味）。 |
| `annotationVars`（99） | 收集表达式里标注用到的类型变量集合。 |
| `exprChildren`（107） | 列出表达式的直接子表达式（遍历用）。 |
| `mapAnnotations`（124） | 对表达式里所有标注类型应用一个变换。 |
| `dictHeadName`（144） | 从 `DictCall` 取被调头名。 |
| `dictCallHints`（151） | 为字典调用错误生成提示（候选列表）。 |
| `callSpine`（181） | 把 `f a b c` 拆成 `(f, [a,b,c])`（调用脊）。 |

### 10.7 TypeCheck/Solver.hs —— 合一与效应行求解引擎（397 行，最底层）

这是整个类型系统的引擎。分几组：

**(A) 变量分配与替换**
| 函数 | 详解 |
|------|------|
| `fresh`（23） | `TVar <$> freshHole`：分配一个新类型变量。 |
| `freshEff`（26） | `Eff S.empty . S.singleton <$> freshHole`：分配一个新**效应行变量**。 |
| `freshHole`（86） | 从 `Counter.nextId` 取号并自增（所有新鲜 id 的来源）。 |
| `apply`（29） | 把 `TSub` 应用到类型（`TVar i` 查表；`TFunc` 还要应用到效应行的 `effParams`）。 |
| `applyEff`（73） | 把替换应用到效应行的参数类型。 |
| `compose`（92） | 替换复合：`compose s2 s1 = M.union (M.map (apply s2) s1) s2`（先 s1 后 s2，标准合一复合）。 |

**(B) 合一（unification）**
| 函数 | 详解 |
|------|------|
| `unify`（95） | 入口：先 `apply` 当前替换化简两端，再 `unifyCore`。 |
| `unifyCore`（107） | **结构合一大 case**：同基类型→空替换；`TVar` → `bindVar`；`TList/TTuple/TCon` → 递归合一各分量并 `compose`；**`TFunc a e b`** → 顺序合一：先 `unifyEffParams`（效应参数）、再 `unifyEff`（效应行）、再 arg、再 res（这个顺序很重要，效应参数先定才能正确比效应行）。类型构造子不同→`TypeMismatch`。 |
| `bindVar`（174） | 绑定类型变量：`bindVar i (TVar j)` 特判（避免自绑）；否则 **occurs check**（`i occursIn t` 则报 `OccurCheck`，防无限类型），成功返回 `M.singleton i t`。 |
| `occursIn`（204） | 变量是否出现在类型里（occurs check 用）。 |

**(C) 效应行求解（R4 的灵魂）**
| 函数 | 详解 |
|------|------|
| `unifyEff`（130） | 合一两个效应行：对齐标签、变量、参数，产生 `effRels` 等式约束。 |
| `unifyEffParams`（158） | 合一参数化效应的类型参数（如 `State a` vs `State Int` → `a := Int`），返回类型替换。 |
| `normalizeEff`（164） | 规范化效应行（用 `resolveEff` 化到最简形式），返回 `(TSub, Eff)`。`inferExpr` 每次都会对结果效应 `normalizeEff`。 |
| `resolveEff`（179） | **效应行解析**：`Counter -> Eff -> Eff`。用 `effDefs` 把行变量替换成定义，用 `effRels` 反复重写，`canonical` 化成规范形（标签排序、去重、合并）。这是「效应行等式求解」的实现。 |
| `checkEff`（273） | 检查一个效应行是否被另一个「涵盖」（子效应关系）。 |
| `freezeType`（263） | 把类型「冻结」：应用 `effTypeSub` + `resolveEff`，得到最终展示/存储用的类型。 |

**(D) 方案、泛化、约束**
| 函数 | 详解 |
|------|------|
| `instantiate`（215）/`instantiateFull`（218） | **实例化方案**：给 `Scheme` 的每个全称变量 `sVars` 分配新 `fresh` 变量，返回替换后的 (约束, 类型)。`Full` 版还返回初始效应，并把 `sEffCs`（效应约束）**推进 `effRels`**（成为待求解等式）。用 `Var` 时就是靠它把多态函数特化到具体类型。 |
| `generalize`（225） | **泛化**：`TEnv -> [Pred] -> T' -> S'`。把不在环境里自由出现的类型变量全称量化（`sVars = ftv t 减去 ftvTEnv env`），构造 `Scheme`。 |
| `dedupPreds`（230） | 约束去重（`S.toList . S.fromList`）。 |
| `ftv`（233）/`ftvPred/ftvS'/ftvTEnv/ftvEff` | 求各结构的自由类型变量集合（泛化时算 `sVars` 用）。 |
| `compatible`（292） | **协变/逆变的函数子类型检查**：`act` 兼容 `expe`？函数类型时参数逆变、结果协变，同时 `checkEff`。 |
| `applyFunction`（303） | 应用一个函数类型到参数：`TFunc a e b` 应用 `arg`（合一 a 与 arg，检查效应 e）返回 `b`；若被应用的不是函数类型报 `TypeMismatch`。 |
| `joinTypes`（334） | 求两个类型的「连接」（if/then/else 两分支、match 多臂的结果类型合并）。相同→原样；能合一→合一结果。 |

**(E) 效应声明校验、关系闭包、错误恢复**
| 函数 | 详解 |
|------|------|
| `operationSchemes`（315） | 从 `EffDecl` 造操作的 `TEnv`：每个操作 `Forall (ftv ty) [] ty`，并把效应标签并进操作类型的效应行。 |
| `validateEffDecl`（324） | 校验效应声明：不能重定义 `Console/FileIO`（宿主的），操作必须是函数类型。 |
| `relationClosure`（366） | **效应关系闭包**：`Counter -> Set Int -> Set Int`。从一组行变量出发，沿 `effRels` 求闭包（哪些行变量互相关联）。`closeBinding` 用它决定泛化时哪些效应变量能一起量化。 |
| `validateEffects`（376） | 全局校验效应约束是否可满足。 |
| `withInferRollback`（388） | **错误恢复**：跑一段动作，出错时把 `Counter` 恢复到进入前的状态，**但保留推进后的 `nextId`**（避免重号）。 |
| `tryInfer`（396） | `(Right <$> withInferRollback act) catchError (return . Left)`：把可能失败的推断转成 `Either`，用于「试一下，不行就换条路」（如隐式候选匹配）。 |

### 10.8 TypeCheck/Infer.hs —— 推断引擎（576 行，核心）

这是把上面所有工具串起来的地方。按调用层次：

**(A) 单表达式入口**
| 函数 | 详解 |
|------|------|
| `typeChecker`（33） | 单表达式类型检查入口（`:t` 命令用）：`TEnv -> E' -> m (TSub, T')`。 |
| `typeOfExprWE`（41） | 带效应的表达式类型：返回 `(S', Eff)`，`withInferRollback` 包住。 |
| `typeOfExprWECore`（44） | 其实现体。 |
| `inferExpr`（57） | **主推断入口**：`TEnv -> IEnv -> E' -> m InferRes`。= `inferExprCore` + 对结果效应 `normalizeEff`。 |

**(B) `inferExprCore`（63）—— 表达式大 case（最核心）**

对 `E'` 每种构造子的推断实现：
- **`Var n`**：查 `env` 拿 `Scheme` → `instantiate` 特化 → 为它的每个 `Pred`（隐式约束）造一个 **`Wanted` 洞** → 若被应用则插 `ImpHole`。这是隐式参数的入口。
- **`ListLit es`**：递归推断各元素，`foldM` 逐一 `unify` 成同一类型 `a`，结果 `[a]`。
- **`TupleLit es`**：递归各分量，组 `TTuple`。
- **`BOpr o a b`**：按运算符查期望类型（算术 `Int->Int->Int`、比较 `Int->Int->Bool` 等），`compatible` 检查两操作数。
- **`Let/StrictLet`** → `inferLet`（232）。
- **`Handle`** → `inferHandler`（345）。
- **`DictCall`** → `inferDictCall`（275）。
- **`If b t f`** → 检查 `b : Bool`，`joinTypes`（合并 then/else 类型）。
- **`Lambda x body`** → 给 `x` 分配 `fresh` 类型变量，推断 body，结果 `TFunc x irNow body`（`irNow` 是当前效应行）。
- **`App f a`** → 推断 `f` 和 `a`，`applyFunction`（把 f 的类型应用到 a）。
- **`Match s arms`** → `inferArm`（255）fold 各臂，`checkNonExhaustive`（穷尽性）。
- **`AnnT e ty`** → 推断 `e`，`compatible` 到标注 `ty`。
- **`At sp e`** → `addSpan`（记录位置，递归推断 e）。
- **`ImpHole h`** → 洞本身（等 `fillHoles` 填）。
- **`ILit/BLit/SLit`** → `TInt/TBool/TString`。

**(C) 复合推断**
| 函数 | 详解 |
|------|------|
| `inferLet`（232） | 推断 `let n = rhs in body`：先推断 rhs，泛化成局部 scheme 加进 env，再推断 body。递归定义会打结。 |
| `inferArm`（255） | fold 一个 match 臂：`patType` 推断模式 → 模式绑定加进 env → 推断臂体 → `unify` 到累积的结果类型 → `unionEff` 合并效应。返回 `(TSub, [Wanted], Maybe 结果类型, 已完成臂, 累积效应)`。 |
| `inferDictCall`（275） | 推断显式字典调用 `f {d1} {d2} a b`：推断头，把字典参数按类型 `compatible` 匹配到头的隐式约束。 |
| `inferHandler`（345） | **效应处理器推断**（重点，之前修过 bug）：`inferHandler env ienv body n ts ret retBody arms`。步骤：① 拿 `denv`；② 查效应 `n` 的操作声明 `denvOperations`，**查不到就 fallback**：`Console` → `consoleEffDecl`、`FileIO` → `fileIOEffDecl`（从 Builtin.hs 导入，**不再本地硬编码**，这是修复点）、其它 → `reject`；③ 推断 body（带效应 `{n ...}`）；④ 检查每个操作臂覆盖恰好一次（`cover each operation exactly once`）；⑤ `ret` 分支和 `return` 类型合并；⑥ **`removeEff n`**：从结果效应里删掉 `n`（handler 消除了它处理的效应）；⑦ 操作臂里续体 `k` 的类型是「继续 body 剩余计算」的函数。 |

**(D) 绑定组检查与泛化（收尾）**
| 函数 | 详解 |
|------|------|
| `inferBindingGroupWithNotes`（417） | **一组互相递归定义的检查**（`Bool` 表示是否标注模式）。核心是 `settle` 不动点循环（**fuel=64**）：反复推断定义体，直到隐式 scheme **收敛**（不再变化）。用 `nameGroups`（给洞起名）/`groupWanted`（分组）/`deferWanted`（延迟未定洞）驱动隐式解析的迭代。 |
| `widenAnswer`（459） | 放宽「答案类型」（把某些具体化的东西重新打开成变量，供泛化）。 |
| `closeBinding`（464）/`closeBindingWithOpen`（467） | **收尾/泛化**（关键）：`closeBinding = closeBindingWithOpen S.empty`。步骤：① 用 `relationClosure` 算效应变量的关联闭包；② 对结果类型做**效应变量默认化**（把仍未定的效应行变量默认成 `emptyEff`，即「假设纯」）；③ `resolveWanted`（解剩余隐式约束）；④ `fillHoles`（填洞，`Left` 未填洞 → `EscapedImpHole`）；⑤ 把隐式参数用 **`Lambda` 包起来**（elaboration：`f {x} = body` → `f = Lambda $x -> body`）；⑥ `generalize` 泛化成 `Scheme`，填 `sDictCall/sEffCs/sInitEff`。返回 `(S', elaborated E')`。 |
| `inferDeclBodyPrepared`（536） | 推断一个已 `prepareDecl` 的声明体：`ExplicitParam x mty` → 给 x 分配 `fresh` 变量并 `Lambda` 包裹；**若有标注 `mty`，先 `checkTypeCons` + `unify(变量, 标注)` 再推断 body**，于是 body 看到具体类型（`(r :: Person).name` 走单态解析，def 不泛化）；`ImplicitParam x ty` → `checkAbilityType`（校验是 ability）+ 造 `Wanted`（该隐式参成为一个待解析约束）。 |
| `rigidEscapes`（568） | 检查一组「刚性」名字（不可逃逸的变量）是否出现在类型里（若出现说明它们逃逸出了作用域 → 错误）。 |

---

### 10.9 字段多态子系统（Stage 1/2，散在 Infer + Declarations + Pretty）

字段访问/更新从「解析期全局一名一义」改成「类型检查期按 scrutinee 具体类型解析」，并支持多态。核心函数：

| 函数 | 详解 |
|------|------|
| `headMarker`（Infer） | 剥掉外层 `At` 拿 head 变量名，识别 parse 期留下的 `$fld@x`/`$upd@...` marker。`inferExprCore` 的 `App` 分支据此分派到字段处理。 |
| `dataFieldInfo`（Infer） | 按 (data 名, 字段名) 从 `denvFields` 查出 (构造子, 下标, 元数, 是否单构造子)；查不到抛 `FieldError`。 |
| `projectionExpr`/`setterExpr`（Infer，纯函数） | 构造投影 `Match`（取第 idx 位）/ 单字段 setter 闭包（`lambda v -> lambda r -> r{f=v}`，其余位用旧值重建）。非单构造子带 `$fieldErr` 兜底臂。 |
| `inferFieldAccess`（Infer） | 字段访问 `r.f`：scrutinee 是具体 `TCon d _` → 构造投影 Match **再重新 `inferExpr`**（让 `patType` 自动算字段类型）；是抽象 `TVar rho` → 生成 getter 约束 `$get_f rho alpha` + 字典洞，body 用 `App (ImpHole h) scrut`（多态投影）；其它（Int/List…）→ `FieldError`。 |
| `inferFieldUpdate`（Infer） | 字段更新 `r{f=v}`：具体 → 构造更新 Match 重推断（多字段须同构造子）；抽象 `TVar rho` → 每字段生成 setter 约束 `$set_f rho beta` + 洞，链式拼成 `setx vx (sety vy r)`。 |
| `resolveFieldWanted`（Infer，接在 `inferExpr` 里） | **字段多态的关键**：字段约束带一个「输出」类型（字段类型），而隐式搜索只解闭合输入约束。所以每次 `inferExprCore` 后扫描 `Wanted`，凡记录类型已具体成 `TCon d args` 的字段约束，就查字段类型 FT、`unify` 输出变量:=FT、按 d 构造 getter/setter 闭包 `replaceSolvedHoles` 填洞、丢弃该约束；仍抽象的原样保留给 `closeBinding` 泛化成隐藏字典参数。 |
| Pretty 行记法 | `printT'` 把合成 ability `$get_x [rho,alpha]`/`$set_x [rho,alpha]` 渲染成记录行记法 `{x : alpha | rho}`（读）/`{x := alpha | rho}`（写）；`prettyS'` 对字段 pred 隐去自动字典名；`UnknownTypeCtor` 遇到 `$get_x`/`$set_x` 渲染成「No record type in scope has a field `x`」而非泄漏内部名。 |

数据流一句话：parser 留 marker → `inferExprCore` 拦截 → 具体类型走 `projectionExpr`/更新 Match（Stage 1）→ 抽象类型发字段约束 + 洞（Stage 2）→ `resolveFieldWanted` 在类型具体化时即时解出输出类型并填字典 → 仍抽象的由 `closeBinding` 泛化进 scheme。合成 ability 由 `programChecker` 经 `buildFieldAbilities` 注入 DEnv，只为过 `checkAbilityType`，字典本身是现场构造的裸投影/setter 函数（不是 ability 记录）。

---

## 11. 改代码时的注意事项（踩坑清单）

1. **FileIO/Console 签名只有一处真值**：`Builtin.hs`（`readFileTy/writeFileTy/consoleEffDecl/fileIOEffDecl`）。改运行时行为要同步 `Main.hs` 的 host 函数和 `docs/LanguageReference.md`。
2. **`emptyDEnv.denvOperations` 是空的**：所以 `inferHandler` 对宿主效应总走 fallback 分支（Infer.hs:346）。若在 fallback 里改签名，就是改宿主效应的类型。
3. **`buildProgramDEnv` 的 `denvEffs` 只含 Console 不含 FileIO**（Declarations.hs:77），和 `emptyDEnv`（两个都含）不一致——这是个历史遗留点，动效应相关代码时留意。
4. **nullary 定义被 EAGER 求值**（Eval.hs SCC 行为）：`def x = do{ ...eff... }` 在绑定时就求值，不在任何 handler 内 → `UnhandledEff`。要延迟就加参数。
5. **resume-once token 永不回收**（`rtResumeUsed`，DN-011 记录的已知限制）：不要单方面「修」它，属于设计取舍。
6. **两趟隐式解析**（TypeCheck.hs:84）和 **settle fuel=64**（Infer.hs:417）是不动点迭代——改隐式相关逻辑要理解为什么需要多趟，别简化成单趟。
7. **`withInferRollback` 保留 nextId**（Solver.hs:388）：回滚状态但不回滚计数器，避免重号。别改成完全回滚。
8. **TFunc 的合一顺序**（Solver.hs:107）：effParams → eff → arg → res。改这个顺序会破坏参数化效应的推断。
9. **字段解析在类型检查期，不在解析期**（§10.9）：`$fld@`/`$upd@` marker 必须活到 `inferExprCore`；`FieldTab` 是 `Map Text [FieldInfo]`（一名多格）。改字段相关逻辑别退回解析期脱糖，否则跨 data 同名字段会重新歧义。
10. **合成字段 ability 名带 `$` 前缀**（`$get_x`/`$set_x`）：`usedFields` 收集时必须跳过 `$`-前缀，否则跨批会 `$get_$get_x` 无限增殖。字典是现场构造的裸函数，不是 ability 记录——别试图按 ability 记录去 `match` 它。
11. **参数标注 `(x :: T)` 在推断 body 之前 unify**（Infer `inferDeclBodyPrepared`）：顺序很关键——先钉住参数类型，body 里的字段访问才看到具体类型、走单态。别挪到 body 之后。
12. **Spec.hs 只启用了 `OverloadedStrings`**（没有 `LambdaCase`）：写测试用 `\r -> case r of`，别用 `\case`。
13. 测试：`cabal test`；黄金测试：`bash test/run-golden.sh`（可用 `INTERP=` 指定 exe 路径）。当前基线 **314 examples / 0 failures**。

---

## 12. 文件-职责一览表（速查）

| 文件 | 一句话职责 |
|------|-----------|
| `src/Interp/Types.hs` | 所有数据类型定义（语法树、类型、值、状态、错误）。 |
| `src/Interp/Parser.hs` | 源码 → AST，含运算符优先级和记录脱糖。 |
| `src/Interp/Synonym.hs` | 类型同义词展开（带环检测）。 |
| `src/Interp/Qualify.hs` | 模块名限定（加 `Alias.` 前缀）。 |
| `src/Interp/Pretty.hs` | 打印类型/值/错误（带源码位置）。 |
| `src/Interp/Builtin.hs` | 内建原语环境 + 宿主效应签名（真值源）。 |
| `src/Interp/TypeCheck.hs` | 类型检查编排（两趟隐式 + SCC + 错误恢复）。 |
| `src/Interp/TypeCheck/Infer.hs` | 推断引擎（表达式大 case + 绑定组 + 泛化）。 |
| `src/Interp/TypeCheck/Solver.hs` | 合一 + 效应行求解引擎（最底层）。 |
| `src/Interp/TypeCheck/Common.hs` | 共享工具（效应合并/删除、错误注解）。 |
| `src/Interp/TypeCheck/Declarations.hs` | 声明环境构建与校验（data/eff/ctor）。 |
| `src/Interp/TypeCheck/Pattern.hs` | 模式类型推断 + 穷尽性检查。 |
| `src/Interp/TypeCheck/Implicit.hs` | 隐式字典解析 + 候选校验。 |
| `src/Interp/TypeCheck/Elaboration.hs` | 洞填充 + AST 改写（elaboration）。 |
| `src/Interp/Computation.hs` | 效应计算 monad + 深处理器 `handleC`。 |
| `src/Interp/Eval.hs` | 求值器（表达式/程序级 + SCC + host）。 |
| `app/Main.hs` | REPL / 文件执行 / 模块系统 / 批处理管线。 |
