# test/rev/README.txt —— 临时工作区（影子副本 / 验收脚本 / 编出来的 exe）

这个目录**整体被 `.gitignore` 忽略**（`.gitignore:5` 忽略 `test/`），是临时工作区，
不是项目的一部分。用完 `rm -rf test/rev` 即可，不影响仓库。

**本轮的入口是下面「2026-09-30 排查轮」那一节。**

---

## 2026-09-30 排查轮：这台机器上 bash 用不了，测试改成 PowerShell

### 为什么换了工具链

`bash` 和 `wsl` 在这台机器上都被策略挡住：

- Git Bash：`bash.exe: *** fatal error - couldn't create signal pipe, Win32 error 5`
- WSL：`Wsl/E_ACCESSDENIED`

所以 `test/*.sh` 与 `test/rev/probe-*.sh` **一个都跑不了**。现在的做法是：

| 原 bash 脚本 | PowerShell 等价物 | 说明 |
|---|---|---|
| `test/run-spec.sh` | **暂缺** | 走 `cabal test`（本机 `dist-newstyle/cache` 被沙箱拒写，所以也跑不了；见「已知缺口」） |
| `test/run-golden.sh` | `build-tmp\golden.ps1` | 金标 + REPL 检查 + 两项体检 |
| `test/rev/probe-str.sh` | `build-tmp\probe-str.ps1` | 字符串（43 条） |
| `test/rev/probe-op.sh` | `build-tmp\probe-op.ps1` | 自定义操作符（见该轮报告） |
| `test/rev/probe-imp.ps1`（新） | `build-tmp\probe-imp.ps1` | 隐式参数（本轮新增） |
| `test/rev/probe-x.ps1`（新） | `build-tmp\probe-x.ps1` | 跨特性接缝（本轮新增） |
| `test/rev/probe-rec.sh` / `probe-syn.sh` / `probe-r3.sh` | 未移植 | 需要「同一进程里改文件再重载」，本轮的替代方案走文件参数 |

### 复现命令

```powershell
# 1) 编一个影子 exe（不改 src/app/utils）。产物在 build-tmp\ 下。
& test/rev/build.ps1 -Out 'build-tmp\v1.exe' -ObjDir 'build-tmp\obj-v1'

# 2) 跑金标 + REPL + 体检
$env:PSExecutionPolicyPreference='Bypass'
& build-tmp\golden.ps1 -Exe 'build-tmp\v1.exe'

# 3) 跑单个探针
& build-tmp\probe-str.ps1 -Exe 'build-tmp\v1.exe'
```

### 这台机器上踩过的坑（**下一个人直接用，别再踩**）

1. **`ps1` 必须是纯 ASCII。** PowerShell 在这里按 ANSI/GBK 读脚本，
   无 BOM 的 UTF-8 中文注释会被拆坏成语法错误（症状：
   `字符串缺少终止符` / `必须在“-”运算符后面提供一个值表达式`）。
   中文说明一律写在 `.md` / `.txt` 里，脚本里只留英文。
2. **不要用 `-replace` 做「替换成一个反斜杠」**：`-replace` 的替换串是正则替换串，
   `\` 是引用引导符。要用 `.Replace(a, b)` 字面替换，且反斜杠写 `[string][char]92`
   最保险。
3. **`$Input` 是 PowerShell 自动变量**，别拿它当函数参数名（会和 pipeline 输入打架，
   症状是参数绑定莫名失败）。
4. **`[ValidateSet]` + 位置参数传 hashtable 会炸**：`@('a','b','c')` 在 PowerShell
   里会被**展平**成字符串数组。测试表要用 `@{ n=...; w=...; k=...; s=... }`。
5. **不要往 `test/rev/` 或 `build-tmp/` 的*子目录*里写新文件**：沙箱对新建目录/文件的
   ACL 配置会失败（`permission denied`）。产物放 `build-tmp\` 这一层，**不要预先删
   `obj-*` 目录**（删 ghc 生成的 `.hi` 会被拒）；要强制重建就换一个 `-ObjDir` 名字。
6. **`Get-Content x | & exe` 是最可靠的喂 stdin 方式**，但**在 `Start-Job` 里会失效**
   （子进程拿到 bare EOF）。本轮的探针一律走**文件参数**（`& exe prog.txt`），
   因为文件模式同样逐语句回显，而且超时/捕获都好控制。
   .NET 的 `StandardInput.Write + Flush + Close` 与 `cmd /c exe < file` **都不可靠**：
   子进程只看到 EOF（症状：只有提示符、空行和 `Bye.`，什么输出都没有）。
7. **手工验证时别被 PowerShell 自己的转义骗了**：`"a\b"` 这种字符串经 `Out-String`
   显示成 `a\\b`。要核对**源文件字节**就用
   `[System.IO.File]::ReadAllBytes(path)`，别用控制台回显。
8. `:t` / `:load` / `:q` **只在 REPL 里有意义**。文件模式下以 `:` 开头的行是解析错误
   （见本轮报告的 F-4，值得改文案）。所以 `.ps1` 探针里不要放 `:t`。

---

## 更早的历史记录（保留，供对照）

下面这些是 bash 工具链时代的记录。**补丁内容与设计结论仍然有效**，
但里面的复现命令在这台机器上跑不了（`bash`/`wsl` 被挡），要按上面的 PowerShell 版重跑。

### 现状（2026-09-27，`;` 迁移这一轮）

**`test/` 已整体迁到 `;` 语法**（换行不再是语句终结符，`;` 才是）。三条验收命令：

    test/run-spec.sh                              # 期望 173 例 0 失败
    INTERP=<exe> test/run-golden.sh               # 期望「基线全绿」（含两项体检）
    INTERP=<exe> test/rev/check-fixes.sh          # 期望「全对（48 条）」

⚠️ **正在运行的 `interp.exe` 会占住 exe 文件**，`cabal build` 链接步会报
   `ld.lld: Permission denied`。绕开的办法是编一个**别的名字**的 exe（见上面的 build.ps1）。

### 编一个影子 exe

原命令（bash/WSL 环境）：

    export PATH="/mnt/c/ghcup/bin:$PATH"
    cd /mnt/e/_love/interepter
    rm -rf test/rev/obj-x
    ghc-9.6.7.exe -Wall -o 'test\rev\x.exe' -outputdir 'test/rev/obj-x' \
      -isrc -iapp \
      -package-db 'C:\Users\78055\cabal_store\store\ghc-9.6.7\package.db' \
      -package text -package containers -package mtl -package megaparsec \
      -package parser-combinators \
      'app\Main.hs' 'src\Interp\Types.hs' 'src/Interp\Pretty.hs' 'src/Interp/Parser.hs' \
      'src/Interp\Eval.hs' 'src/Interp\TypeCheck.hs' 'src/Interp\Builtin.hs'

**已换成 `test/rev/build.ps1`**（参数见上）。它比原命令多编了
`src/Interp/Qualify.hs` 与 `src/Interp/Synonym.hs` —— 原命令漏了这两个模块，
在阶段 3（类型同义词）之后是编不过的。

### 各阶段影子与补丁

| 阶段 | 影子 | 补丁 | 状态 |
|---|---|---|---|
| `;` 终结符 | `s4.exe` | `parser-semicolon.patch` | ✅ 已进 `src` |
| 限定名 `import … as A` | `cur5/cur6/cur7.exe` | `parser-swap-fix` / `qualify-var-ctors` / `import-line-fix` / `repl-import` | ✅ 已进 `src` |
| 同行多语句显示串行 | `curT.exe` | `same-line-types.patch` | ✅ 已进 `src` |
| per-SCC def 错误恢复 | `s2c.exe` | `stage2b-scc.patch` | ✅ 已进 `src` |
| R3 幂等 = 重载 | `r3.exe` / `r4.exe` | `stage2b-r3.patch` | ✅ 已进 `src` |
| 阶段 3 类型同义词 | `syn.exe` | `stage3-synonym.patch` | ✅ **已进 `src`**（源码核过） |
| 阶段 4 字符串 | `str.exe` | —— | ✅ **已进 `src`**（`TString`/`SLit`） |
| 阶段 5 自定义操作符 | `op.exe` | —— | ✅ **已进 `src`**（`Tabs`/`StmtInfix`/`scanFixities`） |
| 阶段 6 Record | `rec.exe` | —— | ✅ **已进 `src`**（`sFields`/`$fld@`…） |
| 阶段 7 扫描器统一 + 行尾注释 | 无 | 无 | ❌ **唯一没做的一项** |

> ⚠️ `TODO/15-剩下的活.md` 里「阶段 3/5/6 补丁待抄」的说法**已经过时** ——
> 对着源码核过，那几项都在 `src/` 里了。

### 老语法的快照（对照用，别删）

    Spec.hs.head-green   迁 `;` **之前**、且债已清干净的那版
    test.txt.bak         迁 `;` 之前的 test.txt
    prelude.txt.bak      迁 `;` 之前的 utils/prelude.txt（0 个分号）
    run-golden.sh.bak    迁 `;` 之前的金标脚本
    Spec.hs.bak          清债之前的 Spec.hs（最早的一版）
