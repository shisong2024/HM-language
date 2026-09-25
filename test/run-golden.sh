#!/usr/bin/env bash
#
# interp 金标回归测试。
#
# 特点：不需要 cabal、不需要 hspec，直接跑已构建的 exe —— 秒级、零依赖。
#      细粒度断言在 test/Spec.hs（用 test/run-spec.sh 跑），两者互补：
#        run-spec.sh    测库 API 的语义（优先级、泛化、Span、渲染器）
#        run-golden.sh  测**端到端 stdout**，覆盖 Main.hs 的接线与格式
#
# 用法：
#     test/run-golden.sh
#     test/run-golden.sh --regen          # 用当前 exe 重新生成三个金标文件
#     INTERP=/path/to/interp.exe test/run-golden.sh
#
# 退出码：0 = 基线全绿；1 = 基线有回归；2 = 找不到 exe。
#
# 三个语料：
#   test.txt                      正确行为基线，必须一直通过
#   test/parse-errors.txt         刻意写坏的行。方案甲（整文件解析）下解析错误是
#                                 整文件级的，所以这类语料一个文件只能放一条 ——
#                                 看到第一条错误就停了。
#   test/known-bugs.txt           已知缺陷，金标记的是"当前错误输出"。
#                                 所以它一旦"失败"，说明有 bug 被修好了 —— 那是好消息，
#                                 此时应更新 test/known-bugs.current.txt。
#
# 注意：Windows 上 exe 输出是 CRLF，这里统一过滤 \r 后比较，
#      所以金标文件存的是 LF，换到 Linux 构建也能用。

set -uo pipefail

cd "$(dirname "$0")/.." || exit 2

EXE="${INTERP:-dist-newstyle/build/x86_64-windows/ghc-9.6.7/interp-0.1.0.0/x/interp/build/interp/interp.exe}"

if [ ! -x "$EXE" ]; then
    echo "找不到解释器：$EXE" >&2
    echo "先跑 cabal build，或用 INTERP=/path/to/interp.exe 指定。" >&2
    exit 2
fi

# 规范化：去掉 CR，保证跨平台可比。
# 注意 `|| true` 是必须的：脚本开了 pipefail，而 runFile 在解析错误时会
# exitWith (ExitFailure 1) —— 那正是我们要的行为，不该让它把
# "stdout 逐字节相同"误判成回归。这里只关心 stdout。
#
# timeout 是兜底：解释器一旦死循环，每个语料都会挂住 30 秒。
actual () { { timeout 30 "$EXE" "$1" < /dev/null 2>&1 || true; } | tr -d '\r'; }

# ---------------------------------------------------------------------------
# 体检：先确认解释器不会在 REPL 里死循环
#
# 判据：喂一行 1+1，15 秒内应当正常跑完（读到 EOF 会自己打 Bye. 退出）。
# 拿不到 124 以外的退出码就当健康。
# ---------------------------------------------------------------------------

printf '1+1\n' | timeout 15 "$EXE" >/dev/null 2>&1
if [ $? -eq 124 ]; then
    echo "★ 解释器在 REPL 里死循环了（喂 1+1 之后 15 秒不返回）。" >&2
    echo "  已知原因：src/Interp/Parser.hs 的" >&2
    echo "      sc = skipMany (void hspace <|> void eol)" >&2
    echo "  hspace 是 takeWhileP，匹配 0 个字符也算成功 —— skipMany 于是永不终止。" >&2
    echo "  修法：import hspace1，写成 skipMany (hspace1 <|> void eol)。" >&2
    echo "  详见 TODO/07-REPL会话.md 与本次测试报告。" >&2
    exit 2
fi

# ---------------------------------------------------------------------------
# 体检 2：失控递归必须被深度守卫拦住
#
# 判据：喂一句自递归定义 + 一句调用，5 秒内应当打印 "Recursion Limited"。
#
# 为什么单独设一道体检：不接守卫时这是个**内存炸弹**，不是普通卡住 ——
# 实测失控递归以 ~250 MB/s 吃内存（6 秒 1.5 GB、14 秒 3.3 GB），
# 会把整台机器拖死，比体检 1 那个纯 CPU 空转的解析器死循环危险得多。
# 超时因此压得比体检 1 短；机器内存吃紧可以再往下调。
#
# 和体检 1 不同，这里**不 exit**：失控递归不影响金标语料本身（test.txt 里
# 没有自递归不终止的用例），所以报警之后继续把语料跑完，最后计入 fail。
#
# ⚠️ 本项只查「守卫够不够硬」，查不出「守卫是不是【过度】触发」——
#    后者表现为本项绿、而 test.txt 基线红（fib 15 报 Depth 1001）。
#    2026-09-23 的修法漏了成功路径的深度还原，正是这个方向，见 TODO/02-栈安全.md。
# ---------------------------------------------------------------------------

rout=$(printf 'def f n = if n == 0 then 1 else f n\nf 1\n' | timeout 5 "$EXE" 2>&1 | tr -d '\r')
rrc=$?
guard_ok=1
if [ "$rrc" -eq 124 ]; then
    guard_ok=0
elif ! printf '%s' "$rout" | grep -qF 'Recursion Limited'; then
    guard_ok=0
    rout_msg="既没超时、也没报 Recursion Limited，实际输出："
fi

# ---------------------------------------------------------------------------
# --regen：拿当前 exe 的输出覆盖金标
#
# ⚠️ 这是"把现状固化下来"，不是"让测试通过"。跑完必须自己 diff 一遍，
#    确认新输出确实是对的 —— 否则就是把 bug 写进基线了。
# ---------------------------------------------------------------------------

if [ "${1:-}" = "--regen" ]; then
    echo "解释器：$EXE"
    echo
    for pair in "test.txt:test/expected.txt" \
                "test/parse-errors.txt:test/parse-errors.expected.txt" \
                "test/known-bugs.txt:test/known-bugs.current.txt"; do
        corpus=${pair%%:*}; golden=${pair##*:}
        actual "$corpus" > "$golden"
        echo "已重写 $golden（$(wc -l < "$golden") 行，来自 $corpus）"
    done
    echo
    echo "现在自己 diff 一遍，确认新输出是对的："
    echo "    git diff -- $golden 2>/dev/null || diff 旧文件 新文件"
    exit 0
fi

fail=0

# 体检 2 的结论在这里计入（它排在 fail 初始化之前，所以单独存了 guard_ok）。
if [ "$guard_ok" -eq 0 ]; then
    echo "★ 体检 2 未过：失控递归没有被深度守卫拦住。" >&2
    if [ "$rrc" -eq 124 ]; then
        echo "  现象：5 秒不返回，期间以 ~250 MB/s 持续吃内存。" >&2
    else
        echo "  现象：${rout_msg:-输出异常}" >&2
        printf '%s\n' "$rout" | sed 's/^/    /' >&2
    fi
    echo "  原因：两种情况 ——" >&2
    echo "    (a) 守卫没接到递归路径上：函数体走的是裸 eval，maxDepth 永远够不到。" >&2
    echo "        失控递归于是变成 ~250 MB/s 的内存炸弹（表现为超时）。" >&2
    echo "        修法：eval 签名加 MonadState Depth；App 里把 eval ec 换成 evalwDepth ec。" >&2
    echo "    (b) 守卫接了，但 RecursionLimited 没渲染出来。" >&2
    echo "        修法：查 Pretty.hs 的 prettyEvalError 有没有 RecursionLimited 分支。" >&2
    echo "  ⚠️ 体检 2 通过【不代表】深度守卫就是对的 ——" >&2
    echo "     还有一种反向缺陷：成功路径没还原深度，守卫会【过度】触发。" >&2
    echo "     那时本项是绿的，但 test.txt 基线会红（fib 15 报 Depth 1001）。" >&2
    echo "     详见 TODO/02-栈安全.md 与 TODO/08-当前问题.md。" >&2
    echo >&2
    fail=1
fi

check () {
    local corpus="$1" golden="$2" label="$3"
    if [ ! -f "$corpus" ]; then
        echo "跳过 $label（找不到 $corpus）"
        return
    fi
    if [ ! -f "$golden" ]; then
        echo "跳过 $label（找不到金标 $golden）"
        return
    fi
    if actual "$corpus" | diff -u "$golden" - >/dev/null; then
        echo "✓ $label"
    else
        echo "✗ $label —— 与 $golden 不一致："
        actual "$corpus" | diff -u "$golden" - | sed 's/^/    /'
        fail=1
    fi
}

echo "解释器：$EXE"
echo

check test.txt test/expected.txt "基线（test.txt）"

check test/parse-errors.txt test/parse-errors.expected.txt "解析错误（test/parse-errors.txt）"

# ---------------------------------------------------------------------------
# 已知缺陷：这里的"失败"是预期的
# ---------------------------------------------------------------------------

echo
if actual test/known-bugs.txt | diff -u test/known-bugs.current.txt - >/dev/null; then
    echo "· 已知缺陷行为未变（test/known-bugs.txt）"
    if [ -s test/known-bugs.txt ]; then
        echo "  里面固化了 $(grep -c . test/known-bugs.txt) 条当前错误的输出"
        echo "  （正文就是 test/known-bugs.txt 本身，配套金标是 test/known-bugs.current.txt）。"
    else
        echo "  当前为空 —— 没有已知缺陷被固化。"
        echo "  新发现的缺陷请写进 test/known-bugs.txt 并重生成 test/known-bugs.current.txt。"
    fi
    echo "  修好一条，这里就会报警 —— 这正是它的用途。"
else
    echo "★ 已知缺陷的输出变了 —— 有缺陷被修好（也可能是有新回归）："
    actual test/known-bugs.txt | diff -u test/known-bugs.current.txt - | sed 's/^/    /'
    echo
    echo "  逐条确认确实是修复后，更新金标："
    echo "      \"$EXE\" test/known-bugs.txt < /dev/null 2>&1 | tr -d '\\r' > test/known-bugs.current.txt"
    echo "  修好的条目应移进 test.txt，并重生成 test/expected.txt。"
fi

# ---------------------------------------------------------------------------
# REPL：逐行喂管道
#
# 这里测的是 app/Main.hs 的接线 —— 会话状态（def 跨行可见）、类型回显、
# 解析错误不中断、以及类型变量编号的**显示**。
#
# ⚠️ 2026-09-25：原来这里断言的是「第二行不是 a0」（拿显示值当计数器单调性的探针）。
# 那条已经过时：`prettyS'` / `prettyT'` 现在在**显示层**按首次出现重编号
# （见 TODO 11 §5），每行都从 a0 起，计数器仍然单调但显示上看不见了。
# 计数器单调性改在 test/Spec.hs 的「计数器跨行单调」里直接断 prNext —— 那才是
# 正确的层次（它是 instantiate 不撞号的前提，而撞号的症状是挂死，不是显示错）。
# 这里改成钉住显示层本身：编号必须从 a0 起，不能把计数器原值（a100+）漏给用户。
#
# 曾经有个缺陷：`line <- getLine` 排在 `done <- isEOF` 之前，最后一行被丢掉。
# 现在的 loop 已经把 isEOF 挪到 getLine 前面，所以这条是正向断言。
# ---------------------------------------------------------------------------

echo
repl_check () {
    local label="$1" input="$2" needle="$3"
    local out
    out=$(printf '%b' "$input" | "$EXE" 2>&1 | tr -d '\r')
    if printf '%s' "$out" | grep -qF -- "$needle"; then
        echo "✓ $label"
    else
        echo "✗ $label —— 输出里找不到「$needle」："
        printf '%s\n' "$out" | sed 's/^/    /'
        fail=1
    fi
}

repl_check "REPL 求值了管道输入的最后一行"        '1+1\n'                 'Value: 2'
repl_check "REPL 跨行保持定义（def 后能用）"      'def f x = x + 1\nf 10\n' 'Value: 11'
repl_check "REPL 里 0 参 def 也能用"              'def x = 1\nx\n'        'Value: 1'
repl_check "REPL 的 :t 打印表达式类型"            ':t 1+1\n'              'Type : Int'
repl_check "REPL 的 :t 认得会话里的 def"          'def f x = x + 1\n:t f\n' 'Int -> Int'
repl_check "REPL 解析错误不退出、继续下一行"      '(((\n1+1\n'            'Value: 2'
repl_check "REPL 类型变量显示从 a0 起编号"        'lambda a -> a\nlambda b -> b\n' 'a0 -> a0'
# 会话跑很久之后计数器会到 100+，显示层必须把它压回 a0/a1（旧行为会打 a1xx）。
repl_check "长会话里 :t 仍从 a0 起编号（不泄漏计数器原值）" \
    ':load utils/list.txt\n:t map\n' '((a0 -> a1) -> ([a0] -> [a1]))'

# 2026-09-23 补：def 体调用另一个 def。
# 之前缺这条，于是 Eval.hs「闭包只捕获本 SCC 的 knot」的缺陷没被发现 ——
# 表现为第二行定义的 def 求值时报「上一行的 def is an unbound variable」。
repl_check "REPL def 体能调用上一行定义的 def" \
    'def f x = x + 1\ndef g x = f x\ng 1\n' 'Value: 2'
repl_check "REPL 用户报的 max2/max3 三行会话" \
    'def max2 a b = if a < b then b else a\ndef max3 a b c = if max2 a b == a then if max2 a c == a then a else c else if max2 b c == b then b else c\nmax3 1 4 3\n' \
    'Value: 4'

echo
if [ "$fail" -eq 0 ]; then
    echo "基线全绿。"
else
    echo "基线有回归，见上面的 diff。"
fi
exit $fail
