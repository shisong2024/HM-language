#!/usr/bin/env bash
#
# R3「同别名同文件 = 重载」的探针。
#
# 用法：bash test/rev/probe-r3.sh test/rev/r3.exe
#       bash test/rev/probe-r3.sh test/rev/pre-scc.exe   # 对照：旧行为（应大面积红）
#
# 每组都是：装模块 → :load 驱动文件（只有一行 import）→ 在 REPL 里查目标表达式
# → **改模块** → 再 :load → 再查同一个表达式。第二次的结果就是 R3 的判据。
# 会话是进程态，所以必须同一个进程里跑完 —— 用子 shell 顺序吐 stdin + sleep 让
# 解释器跟上（exe 是 Windows 二进制，stdin 走管道也能逐行读，实测可靠）。

set -uo pipefail
cd "$(dirname "$0")/../.." || exit 2

EXE="${1:-test/rev/r3.exe}"
[ -x "$EXE" ] || { echo "找不到 $EXE" >&2; exit 2; }

D=test/rev/r3probe
rm -rf "$D"; mkdir -p "$D"
printf 'import mod.txt as Z;\n' > "$D/drv.txt"
pass=0; fail=0

# repl2 <名字> <起手文件命令> <模块文本> <中途改动命令> <目标表达式> <第一次期望> <第二次期望>
repl2 () {
    local name="$1" pre="$2" modtxt="$3" edit="$4" target="$5" want1="$6" want2="$7"
    rm -f "$D"/mod*.txt
    eval "$pre"
    printf '%b' "$modtxt" > "$D/mod.txt"
    local out
    out=$({
        printf ':load %s/drv.txt\n' "$D"; sleep 1
        printf '%s\n' "$target";          sleep 1
        eval "$edit"
        printf ':load %s/drv.txt\n' "$D"; sleep 1
        printf '%s\n' "$target";          sleep 1
        printf ':q\n'
    } | timeout 60 "$EXE" 2>&1 | tr -d '\r')
    # 只取两次查询的输出行；错误行前面带 `行号: ` 前缀，统一抹掉再比
    local got
    got=$(printf '%s\n' "$out" | grep -E 'Value:|Type Error:|Eval Error:' \
          | sed -E 's/^.*(Value:|Type Error:|Eval Error:)/\1/' | sed -n '1p;2p' | tr '\n' '|')
    if [ "$got" = "$want1|$want2|" ]; then
        echo "✓ $name"; pass=$((pass+1))
    else
        echo "✗ $name —— 期望 $want1|$want2，实际 ${got:-（空）}"
        printf '%s\n' "$out" | sed 's/^/    /'
        fail=$((fail+1))
    fi
}

echo "解释器：$EXE"
echo
echo "① 模块的值改了 → 重载必须看到新值（TODO/14 记的那个 bug：原来静默用旧定义）"
repl2 "值改变" ":" 'def zz = 1;\n' \
    "printf 'def zz = 9;\\n' > $D/mod.txt" \
    'Z.zz;' 'Value: 1' 'Value: 9'

echo
echo "② 模块的类型改了 → 重载后新类型生效（不只是值刷新）"
repl2 "类型改变" "printf 'def bb = false;\\n' > $D/mod2.txt" \
    'import mod2.txt as Z2;\ndef zz = Z2.bb;\n' \
    "printf 'def bb = true;\\n' > $D/mod2.txt" \
    'Z.zz;' 'Value: false' 'Value: true'

echo
echo "③ 模块里删掉一个名字 → 重载后必须真的消失（卸干净，不是只覆盖）"
repl2 "删名要消失" ":" 'def zz = 1;\ndef extra = 2;\n' \
    "printf 'def zz = 1;\\n' > $D/mod.txt" \
    'Z.extra;' 'Value: 2' 'Type Error:'

echo
echo "④ data 重定义、构造子被删 → 旧构造子必须消失（DEnv 那半边也要卸）"
repl2 "构造子要消失" ":" 'data T = A | B;\ndef zz = 1;\n' \
    "printf 'data T = B;\\ndef zz = 1;\\n' > $D/mod.txt" \
    'Z.A;' 'Value: Z.A' 'Type Error:'

echo
echo "⑤ 传递依赖：mod → moda → modb，改 modb 后重载 → 必须一起刷新"
repl2 "传递刷新" "printf 'import modb.txt as ZB;\ndef za x = ZB.zb + x;\n' > $D/moda.txt; printf 'def zb = 1;\n' > $D/modb.txt" \
    'import moda.txt as ZA;\ndef zz = ZA.za 0;\n' \
    "printf 'def zb = 7;\\n' > $D/modb.txt" \
    'Z.zz;' 'Value: 1' 'Value: 7'

echo
echo "⑥ 环：moda ↔ modb —— 重载路径不能发散（环里跨引用名字装不上是既有限制，见下）"
# ⚠️ 环里两边都**引用对方的 def** 是装不上的（modb 先加载，那时 moda 的 body 还没进
#    会话 ⇒ `ZA.za is an unbound variable`），这与 R3 无关，是「按依赖顺序加载」的
#    既有限制。所以这里让环只体现在 import 行上，专测重载路径不会无限递归。
repl2 "环不发散" "printf 'import moda.txt as ZA;\ndef zb = 2;\n' > $D/modb.txt; printf 'import modb.txt as ZB;\ndef za = 1;\n' > $D/moda.txt" \
    'import moda.txt as ZA;\ndef zz = ZA.za + 2;\n' \
    "printf 'import modb.txt as ZB;\ndef za = 5;\n' > $D/moda.txt" \
    'Z.zz;' 'Value: 3' 'Value: 7'

echo "⑦ 反例：别名规则不能被「重载」吃掉"
printf 'def zz = 1;\n' > "$D/modb.txt"
printf 'def zz = 2;\n' > "$D/other.txt"
out=$(printf 'import %s/modb.txt as ZB;\nimport %s/other.txt as ZB;\n:q\n' "$D" "$D" \
      | timeout 40 "$EXE" 2>&1 | tr -d '\r')
if printf '%s' "$out" | grep -qF 'alias `ZB` is already used for'; then
    echo "✓ 别名冲突仍报"; pass=$((pass+1))
else
    echo "✗ 别名冲突仍报 —— 实际："; printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi
out=$(printf 'import %s/modb.txt as ZB;\nimport %s/modb.txt as ZC;\n:q\n' "$D" "$D" \
      | timeout 40 "$EXE" 2>&1 | tr -d '\r')
if printf '%s' "$out" | grep -qF 'is already imported as `ZB`'; then
    echo "✓ 同文件换别名仍报"; pass=$((pass+1))
else
    echo "✗ 同文件换别名仍报 —— 实际："; printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi

echo
echo "⑧ 同一个 import 写两遍 → 幂等，不报错、值不变"
repl2 "重复 import 幂等" ":" 'def zz = 1;\n' ":" 'Z.zz;' 'Value: 1' 'Value: 1'
printf 'import mod.txt as Z;\nimport mod.txt as Z;\n' > "$D/drv.txt"
out=$(printf ':load %s/drv.txt\nZ.zz;\n:q\n' "$D" | timeout 40 "$EXE" 2>&1 | tr -d '\r')
if printf '%s' "$out" | grep -qF 'Value: 1' && ! printf '%s' "$out" | grep -qi 'already'; then
    echo "✓ 同一文件里写两遍"; pass=$((pass+1))
else
    echo "✗ 同一文件里写两遍 —— 实际："; printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi

echo
if [ "$fail" -eq 0 ]; then echo "全对（$pass 条）"; else echo "$fail 条不对，见上。"; fi
exit $(( fail > 0 ))
