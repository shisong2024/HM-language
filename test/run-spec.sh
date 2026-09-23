#!/usr/bin/env bash
#
# interp 的 hspec 套件跑测脚本。
#
# interp.cabal 的 test-suite 依赖已经补齐（mtl/text/containers 都在），
# 所以 `cabal test` 现在也能跑。这个脚本用 ghc 直接编，绕开 cabal，
# 好处是能拿它去编 *另一份* src（见下面的 SRC）—— 比如临时验证一个修复。
#
# 用法：
#     test/run-spec.sh              # 编译 + 跑
#     test/run-spec.sh --match foo  # 参数原样透传给 hspec
#
# 环境变量（一般不用设，脚本会自己找）：
#     GHC      ghc 可执行文件（Windows 二进制）
#     PKGDB    package.db 路径（Windows 形式，如 C:\...\package.db）
#     SRC      要编译的源码目录，默认 src。
#              指向一份改了 Eval.hs 的临时副本，就能验证「let 求值」那组是否真的转绿：
#                  SRC=/mnt/c/.../rc/src test/run-spec.sh
#
# 退出码：0 = 全绿；1 = 有用例失败；2 = 环境/编译问题。

set -uo pipefail

cd "$(dirname "$0")/.." || exit 2
ROOT=$(pwd)

# ---------------------------------------------------------------------------
# 1. 找 ghc
# ---------------------------------------------------------------------------

find_ghc () {
    if [ -n "${GHC:-}" ]; then echo "$GHC"; return; fi
    # 优先取版本最高的 ghcup 安装。
    # 注意模式是 ghc-[0-9]*.exe —— 写成 ghc-*.exe 会匹配到 ghc-pkg-9.6.7.exe。
    local c
    c=$(ls -1 /mnt/c/ghcup/bin/ghc-[0-9]*.exe 2>/dev/null | grep -v shim | sort -V | tail -1)
    [ -n "$c" ] && { echo "$c"; return; }
    command -v ghc 2>/dev/null
}

GHC_BIN=$(find_ghc)
if [ -z "${GHC_BIN:-}" ] || [ ! -x "$GHC_BIN" ]; then
    echo "找不到 ghc。设 GHC=/path/to/ghc 再试。" >&2
    exit 2
fi

# ---------------------------------------------------------------------------
# 2. 找 package.db
#
#    cabal 把依赖装在 store 里，不在 ghc 的全局库里，所以必须显式 -package-db。
# ---------------------------------------------------------------------------

find_pkgdb () {
    if [ -n "${PKGDB:-}" ]; then echo "$PKGDB"; return; fi
    local d
    # WSL：/mnt/c/Users/*/cabal_store/store/ghc-*/package.db
    d=$(ls -1d /mnt/c/Users/*/cabal_store/store/ghc-*/package.db 2>/dev/null | sort -V | tail -1)
    if [ -n "$d" ]; then
        # 转成 Windows 形式：ghc 是 Windows 二进制，看不见 /mnt/c
        echo "$d" | sed 's|^/mnt/\([a-z]\)/|\U\1:\\|; s|/|\\|g'
        return
    fi
    # 原生 Windows：$HOME/cabal_store/... 或 %LOCALAPPDATA%\cabal\store\...
    for d in "$HOME"/cabal_store/store/ghc-*/package.db \
             "$HOME"/AppData/Roaming/cabal/store/ghc-*/package.db \
             "$HOME"/AppData/Local/cabal/store/ghc-*/package.db; do
        [ -d "$d" ] && { echo "$d"; return; }
    done
    # 退化：用 ghc 的全局库（如果依赖装在全局库里也能编过）
    echo ""
}

PKG_DB=$(find_pkgdb)
if [ -z "$PKG_DB" ]; then
    echo "⚠️  找不到 package.db，改用 ghc 全局库。若报 'Could not find module'，"
    echo "    请设 PKGDB='C:\\...\\cabal_store\\store\\ghc-9.6.7\\package.db'。" >&2
fi

# ---------------------------------------------------------------------------
# 3. 编译
# ---------------------------------------------------------------------------

OUT=dist-newstyle/spec
mkdir -p "$OUT"

# 先删旧的，否则编译失败时下面 [ -x ] 会看到上一次的 exe，
# 于是拿旧二进制跑出一片虚假的绿。
rm -f "$OUT/spec.exe"

# ghc 是 Windows 二进制，路径要转成 Windows 形式
winpath () { echo "$1" | sed 's|^/mnt/\([a-z]\)/|\U\1:\\|; s|/|\\|g'; }

SRC_DIR=${SRC:-src}
if [ ! -d "$SRC_DIR" ]; then
    echo "SRC=$SRC_DIR 不是目录。" >&2
    exit 2
fi
case "$SRC_DIR" in
    /*) WIN_SRC_DIR=$(winpath "$(cd "$SRC_DIR" && pwd)") ;;
    *)  WIN_SRC_DIR="$SRC_DIR" ;;
esac

WIN_SRC=""
for m in Types Pretty Parser Eval TypeCheck; do
    WIN_SRC="$WIN_SRC $WIN_SRC_DIR\\Interp\\$m.hs"
done
WIN_TEST='test\Spec.hs'

PKGARGS=(-package hspec -package QuickCheck -package text -package containers
         -package mtl -package megaparsec -package parser-combinators)

DBARG=()
[ -n "$PKG_DB" ] && DBARG=(-package-db "$PKG_DB")

echo "ghc     : $GHC_BIN"
echo "package : ${PKG_DB:-<全局库>}"
echo "src     : $SRC_DIR"
echo "编译中…"

# shellcheck disable=SC2086
# -Wall 跟 interp.cabal 的 common warnings 一致，顺带把库的 warning 也带出来
"$GHC_BIN" -Wall -o "$OUT/spec.exe" -outputdir "$OUT/obj" -i"$WIN_SRC_DIR" -itest \
    "${DBARG[@]}" "${PKGARGS[@]}" \
    $WIN_TEST $WIN_SRC 2>&1 | tr -d '\r' | grep -a -v '^\['

if [ ! -x "$OUT/spec.exe" ]; then
    echo
    echo "❌ 编译失败（上面的 error 就是原因）。" >&2
    echo "   常见原因：src/ 里有类型错误 —— 套件会先撞上库的编译错误。" >&2
    exit 2
fi

# ---------------------------------------------------------------------------
# 4. 跑
# ---------------------------------------------------------------------------

echo
# < /dev/null：防止 hspec 以为要读 stdin
"$OUT/spec.exe" "$@" < /dev/null 2>&1 | tr -d '\r'
rc=${PIPESTATUS[0]}

echo
if [ "$rc" -eq 0 ]; then
    echo "✅ 全绿。"
else
    echo "❌ 有用例失败（exit $rc）。"
    echo "   看上面的 Failures 段。若是「已知缺陷」组变红，说明有缺陷被修好了 ——"
    echo "   那是好消息，请更新断言。"
fi
exit $rc
