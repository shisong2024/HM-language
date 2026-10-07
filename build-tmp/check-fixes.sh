#!/usr/bin/env bash
#
# 「补丁验收脚本」——逐条确认 2026-09-25 那批修复的行为对不对。
#
# 用途：把补丁抄进 src/ 之后（或之后又改坏了），跑这个脚本逐条确认。
#       它只测这些点，完整回归请跑：
#           test/run-spec.sh                      # 期望 173 examples, 0 failures
#           INTERP=<exe> test/run-golden.sh        # 必须「基线全绿」（含两项体检）
#
# ⚠️ 2026-09-27 起，输入都是 `;` 语法（换行不再是语句终结符），所以**必须**用
#    打过 test/rev/parser-semicolon.patch 的 exe 跑，否则全是 Parse Error。
# ⚠️ ⑦ 要 test/rev/stage2b-scc.patch，⑧ 要 test/rev/stage2b-r3.patch，
#    ⑨ 要 test/rev/stage3-synonym.patch，⑩ 要 test/rev/stage4-string.patch，
#    ⑪ 要 test/rev/stage5-ops.patch，⑫ 要 test/rev/stage6-record.patch。
#    全打上才是 48/48（影子 test/rev/rec.exe）；缺哪个，哪一区红。
#
# 现有分区：① 穷尽性 ② 行内多语句 ③ 解析/求值 ④ … ⑤ 同行定型 ⑥ 诊断提示通道
#           ⑦ per-SCC def 恢复 ⑧ R3 幂等=重载 ⑨ 类型同义词 ⑩ 字符串
#           ⑪ 自定义操作符 ⑫ Record。
#           **每条断言都先在「打补丁之前」的 exe 上确认过是红的**
#           （见各区注释）—— 不这么做就不知道它到底测没测到东西。
#
# 用法：
#     INTERP=test/rev/s4.exe test/rev/check-fixes.sh   # s4.exe = 工作区 src + 那 2 行补丁
#     INTERP=<别的 exe> test/rev/check-fixes.sh        # 换一个 exe 跑同一套（比如改动前的基线）
#
# 退出码：0 = 全对；1 = 有不对的。
#
# 注：exe 是 Windows 二进制，stdin 必须重定向、输出是 CRLF。

set -uo pipefail
cd "$(dirname "$0")/../.." || exit 2

EXE="${INTERP:-test/rev/cur.exe}"
[ -x "$EXE" ] || { echo "找不到 $EXE（先跑 cabal build，或 INTERP= 指定）" >&2; exit 2; }

tmp="test/rev/.tmp-probe.txt"
tmpout="test/rev/.tmp-probe.out.txt"
trap 'rm -f "$tmp" "$tmpout"' EXIT

# repl <name> <期望：reject|accept> <必须含的文本（可空）> <输入> [必须**不**含的文本]
pass=0; fail=0
repl () {
    local name="$1" want="$2" needle="$3" input="$4" ban="${5:-}"
    local out
    out=$(printf '%b' "$input" | timeout 20 "$EXE" 2>&1 | tr -d '\r')
    local ok=1
    case "$want" in
        # ④b/④c 是解析期就拒（Parse Error），其余是类型检查期（Type Error）。
        reject) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:' || ok=0 ;;
        accept) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:' && ok=0 ;;
    esac
    if [ -n "$needle" ]; then
        printf '%s' "$out" | grep -qF -- "$needle" || ok=0
    fi
    if [ -n "$ban" ]; then
        printf '%s' "$out" | grep -qF -- "$ban" && ok=0
    fi
    if [ "$ok" -eq 1 ]; then
        echo "✓ $name"; pass=$((pass+1))
    else
        echo "✗ $name —— 期望[$want]需含「$needle」${ban:+、需不含「$ban」}，实际："
        printf '%s\n' "$out" | sed 's/^/    /'
        fail=$((fail+1))
    fi
}

# probe_file <name> <期望：reject|accept> <必须含的文本（可空）> <文件内容> [必须**不**含的文本]
#
# 和 repl 的两点区别：
#   1. 走**文件**入口 —— 整个文件是一次提交，REPL 是逐行提交，一批只有一条语句；
#   2. 顺带断言退出码：reject 必须非 0、accept 必须 0。
#      坏文件退出 0 是真实风险（CI 会把坏文件当成功），所以退出码跟着一起断。
probe_file () {
    local name="$1" want="$2" needle="$3" body="$4" ban="${5:-}"
    printf '%b' "$body" > "$tmp"
    # 注意 $? 必须在**没有管道**的那一步取 —— `cmd | tr` 会把退出码换成 tr 的。
    timeout 20 "$EXE" "$tmp" > "$tmpout" 2>&1
    local rc=$?
    local out; out=$(tr -d '\r' < "$tmpout")
    local ok=1
    case "$want" in
        reject) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:' || ok=0
                [ "$rc" -ne 0 ] || ok=0 ;;
        accept) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:' && ok=0
                [ "$rc" -eq 0 ] || ok=0 ;;
    esac
    if [ -n "$needle" ]; then
        printf '%s' "$out" | grep -qF -- "$needle" || ok=0
    fi
    if [ -n "$ban" ]; then
        printf '%s' "$out" | grep -qF -- "$ban" && ok=0
    fi
    if [ "$ok" -eq 1 ]; then
        echo "✓ $name"; pass=$((pass+1))
    else
        echo "✗ $name —— 期望[$want]需含「$needle」${ban:+、需不含「$ban」}，退出码 $rc，实际："
        printf '%s\n' "$out" | sed 's/^/    /'
        fail=$((fail+1))
    fi
}

echo "解释器：$EXE"
echo
echo "① TList 穷尽性：没有 [] 臂就必须拒（&& 比 || 紧那个坑）"
echo "   （这些已进 src/，两个 exe 都该全过；红了就是回归）"

repl "只有 cons 臂 → 拒"          reject 'not exhaustive' 'match [1] with (x: xs) -> x;\n'
repl "同一构造子多臂但缺 [] → 拒"  reject 'not exhaustive' 'data Maybe a = None | Some a;\ndef f xs = match xs with (Some y):ys -> 1 | None:ys -> 2;\n'
repl "补上 [] 臂 → 收"            accept 'Type : Int'     'match [1] with (x: xs) -> x | [] -> 0;\n'
repl "三臂全盖 → 收"              accept 'Value: 2'       'data Maybe a = None | Some a;\ndef f xs = match xs with [] -> 0 | (Some y):ys -> 1 | None:ys -> 2;\nf [None, Some 1];\n'
repl "Bool 缺一半 → 拒（旧有）"    reject 'not exhaustive' 'match true with true -> 1;\n'

echo
echo "② 重定义 data 后，旧构造子必须从会话里消失（sTEnv/sEnv 那半边）"

repl "重定义后旧构造子 unbound"    reject 'unbound variable' 'data T = A | B;\ndata T = B;\nA;\n'
repl "重定义后新构造子仍在"        accept 'Value: B'         'data T = A | B;\ndata T = B;\nB;\n'
repl "原样重定义仍合法"            accept 'Value: A'         'data T = A | B;\ndata T = A | B;\nA;\n'

echo
echo "③ 跨行抢构造子名必须拒"

repl "data U = A 抢 A → 拒"        reject 'Duplicate constructor' 'data T = A;\ndata U = A;\n'
repl "不重名 → 收"                accept 'Type : U'         'data T = A;\ndata U = B;\n:t B;\n'

echo
echo "④ §9.5 这批（已进 src/；理由见 TODO/14 §2 与 test/rev/README.txt）"
echo "   ④a 类型构造子的元数要在**声明处**就查出来"

repl "元数多给 → 声明处报"        reject 'Type T expects 1 argument(s) but got 2' 'data T a = C a;\ndata U = D (T Int Int);\n'
repl "元数少给（裸 T）→ 报"       reject 'Type T expects 1 argument(s) but got 0' 'data T a = C a;\ndata U = D T;\n'
repl "注解里元数不对 → 报"        reject 'Type T expects 1 argument(s) but got 2' 'data T a = C a;\n(C 1) :: T Int Int;\n'
repl "元数对 → 收"                accept 'Value: (D (C 1))' 'data T a = C a;\ndata U = D (T Int);\nD (C 1);\n'
repl "自递归 Tree → 收（不误伤）"  accept 'Type : Tree Int'   'data Tree a = Leaf a | Node (Tree a) (Tree a);\nNode (Leaf 1) (Leaf 2);\n'

echo
echo "   ④b 重复的类型参数名要拒（原来静默退化成 P a b = MkP a）"

repl "重复参数名 → 拒"            reject 'duplicate type parameter a in data P'   'data P a a = MkP a;\n'
repl "不重复 → 收（不误伤）"       accept 'Type : P Int Bool'                      'data P a b = MkP a b;\nMkP 1 true;\n'

echo
echo "   ④c 保留类型名 Int / Bool 不许被 data 占用"

repl "data Int → 拒"              reject 'Int is a built in type name'             'data Int = X;\n'
repl "data Bool → 拒"             reject 'Bool is a built in type name'            'data Bool = Y;\n'
repl "data Integer → 仍可（不误伤）" accept 'Value: Z'                               'data Integer = Z;\nZ;\n'
repl "data IntList = IL Int → 仍可" accept 'Value: (IL 1)'                           'data IntList = IL Int;\nIL 1;\n'

echo
echo "   ④d 构造子签名不再打多余的 Forall（只是显示）"

repl "签名无 Forall、编号从 a0"    accept 'Some : (a0 -> Maybe a0)' 'data Maybe a = None | Some a;\n' 'Forall '
repl "零参构造子就是类型名"        accept 'None : Maybe a0'         'data Maybe a = None | Some a;\n' 'Forall '

echo
echo "   ⑤ 同一行多条语句（分号分隔）—— 2026-09-28 修的 relabel 串行 bug"

# 根因：programChecker 吐出的 Map 是按**语句下标**编号的（对），
# 但 printBatch 按**行号**查表，中间的 relabel 用 M.fromList 把下标重编成行号，
# 同一行的多条语句撞同一个键 → 后者覆盖前者。于是：
#   · 类型显示串（第一条显示成第二条的类型）
#   · 表达式的 Value 也串
#   · 更糟：第一条若是类型错，错误被整条吞掉，ok 还判成通过
repl "同行两条 def 各自定型"      accept 'def a1 : Forall a0. (a0 -> a0)' 'def a1 = lambda x -> x; def b1 = 1;\n' 'def a1 : Int'
repl "同行第一条的类型错不被吞"    reject ''                                '1 + true; 2 + 2;\n'

echo
echo "   ⑥ 诊断提示通道（2026-09-28 阶段 2a：WithNote / CalleeNote）"
echo "      前提是解构 let 的 withSpan 已补回，否则第一条没有 caret"

# 解构 let 的类型错必须**带源码行和 caret**：脱糖出的 Match 若没被 At 包住，
# span 就是 Located Nothing，renderLocated 直接退化成裸消息。
repl "解构 let 的错带 caret"      reject '| let (a, b) = 1 in a;' 'let (a, b) = 1 in a;\n'
repl "脱糖临时名不泄漏"           reject 'Type mismatch'          'let (a, b) = 1 in a;\n' '$let@'

# `match` 的臂一直向右吞：臂体里没加括号的 match 会把后面的 `|` 臂全部吞进去。
# 症状就是「外层臂的模式」被拿去和内层 scrutinee 合。
repl "| 被吞并 → 定向提示"        reject 'absorbed this'          'data T = A | B | C;\ndef f x = match x with A -> match (x, x) with (A, A) -> 1 | B -> 2 | C -> 3;\n'
repl "单臂模式错不误报吞并"        reject 'Type mismatch'          'data T = A | B | C;\nmatch A with (A, A) -> 1;\n' 'absorbed this'

# 应用处的类型错，若被调方是有 scheme 的名字，就把 scheme 点出来
# （用户实际撞的是「foldr 解析到 prelude 的列表 foldr」）。
repl "被调方是 prelude 函数 → 点名" reject '`foldr` is bound here with type' 'foldr 1 2;\n'
repl "被调方是顶层 def → 点名"     reject '`myMap` is bound here with type' 'def myMap f xs = match xs with [] -> [] | h : t -> f h : myMap f t;\nmyMap 1 [2];\n'
# 但单态局部（lambda 绑定 / 递归组成员）的 scheme 就是个裸 a0，说了等于没说 → 不加
repl "单态局部不加噪音"           reject 'Occurs check failed.'   'lambda x -> x x;\n' 'is bound here with type'

# 用 'definition of `h`' 而不是 'the definition of `h`'：src/ 里的措辞是
# "in definition of ..."（没有 the），影子里曾是 "in the definition of ..."。
# 取公共子串，两种措辞都认 —— 断言不该钉住冠词。
repl "def 内的错点名 def"         reject 'definition of `h`' 'def h x = x + true;\n'

echo
echo "   ⑦ per-SCC def 恢复（2026-09-28：一批里每个坏 def 都报，依赖者标未检查）"

# 两个互相独立的坏 def。s2a 撞上第一个就整批中止，而 SCC 的处理顺序不保证是
# 书写顺序（实测它先处理 b2），所以下面两条要**成对**看：无论先撞上哪个，
# s2a 总有一条是红的。s2b 两个都报，两条才同时绿。
twoBad='def b1 x = x + true; def b2 x = x x;\n'
repl "多坏 def：type mismatch 也报"  reject 'Type mismatch: expected Int but got Bool.' "$twoBad"
repl "多坏 def：occurs 也照样报"     reject 'Occurs check failed.'                    "$twoBad"
# 依赖坏 def 的那个不能说成 unbound —— 它压根没被检查，得说清楚。
repl "依赖坏 def → 标「未检查」"     reject 'Not checked: `b1` did not type check.'   'def b1 x = x + true; def d y = b1 y;\n'
repl "同批里好的 def 照样定型"       reject 'def g : '                                'def b1 x = x + true; def g x = x + 1;\n'
# 用到「没检查」的 def 时，必须说「没有类型」而不是「unbound variable」——
# 名字是在作用域里的，说 unbound 会把用户支到错误的方向去查。
repl "用到没检查的 def → 说没类型"   reject 'has no type'  'def b1 x = x + true; def d y = b1 y + 1; d 1;\n' 'is an unbound variable'

# ⚠️ 以上都走 REPL，而 REPL **一行一次 progRun**，所以一批只有一条语句。
#    真正「一整批一起检查」的是文件入口，也才走 printBatch 的逐条打印。
probe_file "文件：坏 def 报错且退出非 0" reject 'Type mismatch'       'def b1 x = x + true;\n'
probe_file "文件：一批两个坏 def 都报"   reject 'Type mismatch'       "$twoBad"
probe_file "文件：依赖者标未检查"        reject 'Not checked: `b1`'   'def b1 x = x + true;\ndef d y = b1 y;\n'
# 类型没定下来的批**不求值**：坏 def 之外的表达式因此没有 Value 可打。
# （s2a 是整批中止，也打不出 Value —— 这条是钉住设计意图，不是新行为。）
probe_file "文件：坏 def 的批不求出值"   reject 'Type mismatch'       'def b1 x = x + true;\n1 + 1;\n' 'Value:'
probe_file "文件：好文件退出 0"          accept 'Value: 42'           'def g x = x + 1;\ng 41;\n'

echo
echo "   ⑧ R3 幂等 = 重载（2026-09-28：同别名同文件不再静默跳过）"

# 这一组要在**同一个进程**里「改文件 → 再 :load → 再查」，repl/probe_file 两个
# 助手都是一次性的，做不到，所以整组外包给 probe-r3.sh（它自己打印 ✓/✗）。
# 判据是 10 条全过：6 条测新行为（对旧 exe 必红），4 条测旧行为不许被吃掉。
r3out=$(bash test/rev/probe-r3.sh "$EXE" 2>&1)
r3n=$(printf '%s\n' "$r3out" | grep -c '^✓')
if [ "$r3n" -eq 10 ]; then
    echo "✓ R3 重载（probe-r3.sh 10/10）"; pass=$((pass+1))
else
    echo "✗ R3 重载 —— probe-r3.sh 只过 $r3n/10："
    printf '%s\n' "$r3out" | grep -E '^(✗|[0-9]+ 条)' | sed 's/^/    /'
    fail=$((fail+1))
fi

echo
echo "   ⑨ 类型同义词（2026-09-29 阶段 3：AST 级急展开）"

# 和 ⑧ 一样，这一组也整组外包 —— 它要 20 条一起看（含跨模块和同进程重载），
# 塞进本脚本会让分区读不出来。probe-syn.sh 自己打印 ✓/✗。
# 判据是 20 条全过：打在没做同义词的 exe 上 20 条全红（已实测）。
synout=$(bash test/rev/probe-syn.sh "$EXE" 2>&1)
synn=$(printf '%s\n' "$synout" | grep -c '^✓')
if [ "$synn" -eq 20 ]; then
    echo "✓ 类型同义词（probe-syn.sh 20/20）"; pass=$((pass+1))
else
    echo "✗ 类型同义词 —— probe-syn.sh 只过 $synn/20："
    printf '%s\n' "$synout" | grep -E '^(✗|[0-9]+ 条)' | sed 's/^/    /'
    fail=$((fail+1))
fi

echo
echo "   ⑩ 字符串（2026-09-29 阶段 4：String 原始类型 + 3 个原语）"

# 同 ⑧ ⑨：整组外包。它要 42 条一起看（字面量/转义/\uXXXX/模式/码点越界/prelude 库/
# 跨模块），而且字符串断言里全是引号和反斜杠，混在本脚本里没法读。
# 判据是 42 条全过：打在没做字符串的 exe 上 42 条全红（已实测，见 probe-str.sh 头注）。
strout=$(bash test/rev/probe-str.sh "$EXE" 2>&1)
strn=$(printf '%s\n' "$strout" | grep -c '^✓')
if [ "$strn" -eq 42 ]; then
    echo "✓ 字符串（probe-str.sh 42/42）"; pass=$((pass+1))
else
    echo "✗ 字符串 —— probe-str.sh 只过 $strn/42："
    printf '%s\n' "$strout" | grep -E '^(✗|[0-9]+ 条)' | sed 's/^/    /'
    fail=$((fail+1))
fi

echo
echo "   ⑪ 自定义操作符 + fixity（2026-09-29 阶段 5：脱糖成函数应用 + 文本预扫描）"

# 同 ⑧ ⑨ ⑩：整组外包。它要 44 条一起看（声明/优先级/结合性/字符集边界/回归/模块接线），
# 而且断言里全是反斜杠和引号。
# 判据是 44 条全过：33 条在阶段 4 的 exe 上实测是红的（见 probe-op.sh 头注）；剩下 11 条
# 是「回归钉子 + 该拒项」，两边都该绿，它们的作用是防止修阶段 6/7 时把这里弄坏。
opout=$(bash test/rev/probe-op.sh "$EXE" 2>&1)
opn=$(printf '%s\n' "$opout" | grep -c '^✓')
if [ "$opn" -eq 44 ]; then
    echo "✓ 自定义操作符（probe-op.sh 44/44）"; pass=$((pass+1))
else
    echo "✗ 自定义操作符 —— probe-op.sh 只过 $opn/44："
    printf '%s\n' "$opout" | grep -E '^(✗|[0-9]+ 条)' | sed 's/^/    /'
    fail=$((fail+1))
fi

echo
echo "   ⑫ Record（2026-09-29 阶段 6：脱糖成构造子应用 + match）"

# 同 ⑧ ⑨ ⑩ ⑪：整组外包。它要 51 条一起看（声明回显/三种脱糖/record 模式/该拒的/
# 运行时错构造子/跨模块与重声明接线），而且断言里全是花括号和大括号。
# 判据是 51 条全过：在阶段 5 的 exe 上实测 0/51（见 probe-rec.sh 头注）—— 这一组没有
# 「两边都绿」的回归钉子，因为连 `data T = A | B { x :: Int };` 这一行都读不了。
recout=$(bash test/rev/probe-rec.sh "$EXE" 2>&1)
recn=$(printf '%s\n' "$recout" | grep -c '^✓')
if [ "$recn" -eq 51 ]; then
    echo "✓ Record（probe-rec.sh 51/51）"; pass=$((pass+1))
else
    echo "✗ Record —— probe-rec.sh 只过 $recn/51："
    printf '%s\n' "$recout" | grep -E '^(✗|[0-9]+/|全对)' | sed 's/^/    /'
    fail=$((fail+1))
fi

echo
if [ "$fail" -eq 0 ]; then
    echo "全对（$pass 条）。完整回归记得再跑 run-spec.sh 与 run-golden.sh。"
else
    echo "$fail 条不对，见上。"
fi
exit $(( fail > 0 ))
