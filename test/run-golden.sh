#!/usr/bin/env bash
#
# interp 金标回归测试。
#
# 特点：不需要改 interp.cabal、不需要 cabal、不需要 hspec，直接跑已构建的 exe。
#      在 test/Spec.hs 能编译之前，这个脚本就是全部的回归保护。
#
# 用法：
#     test/run-golden.sh
#     INTERP=/path/to/interp.exe test/run-golden.sh
#
# 退出码：0 = 基线全绿；1 = 基线有回归。
#
# 两个语料：
#   test.txt                 正确行为基线，必须一直通过
#   test/known-bugs.txt      已知缺陷，金标记的是"当前错误输出"。
#                            所以它一旦"失败"，说明有 bug 被修好了 —— 那是好消息，
#                            此时应更新 test/known-bugs.current.txt。
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

# 规范化：去掉 CR，保证跨平台可比
actual () { "$EXE" "$1" 2>&1 | tr -d '\r'; }

fail=0

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

# 已知缺陷：这里的"失败"是预期的
echo
if actual test/known-bugs.txt | diff -u test/known-bugs.current.txt - >/dev/null; then
    echo "· 已知缺陷行为未变（test/known-bugs.txt）"
    echo "  若你刚修好某个缺陷，请更新金标："
    echo "      \"$EXE\" test/known-bugs.txt 2>&1 | tr -d '\\r' > test/known-bugs.current.txt"
else
    echo "★ 已知缺陷的输出变了 —— 有缺陷被修好（也可能是有新回归）："
    actual test/known-bugs.txt | diff -u test/known-bugs.current.txt - | sed 's/^/    /'
    echo
    echo "  逐条确认确实是修复后，更新金标："
    echo "      \"$EXE\" test/known-bugs.txt 2>&1 | tr -d '\\r' > test/known-bugs.current.txt"
fi

echo
if [ "$fail" -eq 0 ]; then
    echo "基线全绿。"
else
    echo "基线有回归，见上面的 diff。"
fi
exit $fail
