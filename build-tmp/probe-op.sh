#!/usr/bin/env bash
#
# 自定义操作符 + fixity（阶段 5）的探针。
#
# 用法：bash test/rev/probe-op.sh test/rev/op.exe       # 打了阶段 5 的影子
#       bash test/rev/probe-op.sh test/rev/str.exe      # 对照：阶段 4，应大量全红
#
# 五类：
#   A. 声明与注册   —— def 的两种写法、默认 fixity、显式 fixity、REPL 回显、:t
#   B. 优先级/结合性 —— 声明真的在动优先级（不是摆设）、右结合、infix 非结合
#   C. 该拒的       —— 字符集边界（. | : $ #）、改内建 fixity、坏优先级、注释/字符串里的假声明
#   D. 回归         —— -2 ^ 2、-7 / 2（div 是 floor）、比较最松、一元负号不吃用户操作符、++ 仍 infixr 5
#   E. 接线         —— :load 进来的文件里声明的操作符，回到 REPL 还能用
#
# ⚠️ 44 条里 33 条在阶段 4 的 exe（test/rev/str.exe）上实测是红的：
#    `def <+> ...` 直接解析错（def 名还只认变量名）、`:load` 里的操作符认不出。
#    红了才说明它在测东西。
#
# 不支持（本阶段有意不做，见 TODO 阶段 5 的已知取舍）：
#   没有 `(<+>)` 引用、没有段 `(<+> 1)`。用 `lambda x -> x <+> 1` 或包一层 def。
#
# 注：exe 是 Windows 二进制，stdin 必须重定向、输出是 CRLF。

set -uo pipefail
cd "$(dirname "$0")/../.." || exit 2

EXE="${1:-test/rev/op.exe}"
[ -x "$EXE" ] || { echo "找不到 $EXE" >&2; exit 2; }

D=test/rev/opprobe
rm -rf "$D"; mkdir -p "$D"
pass=0; fail=0

# chk <名字> <accept|reject> <必须含的文本> <输入>
chk () {
    local name="$1" want="$2" needle="$3" input="$4"
    local out
    out=$(printf '%b' "$input" | timeout 20 "$EXE" 2>&1 | tr -d '\r')
    local ok=1
    case "$want" in
        reject) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:|Eval Error:' || ok=0 ;;
        accept) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:|Eval Error:' && ok=0 ;;
    esac
    printf '%s' "$out" | grep -qF -- "$needle" || ok=0
    if [ "$ok" -eq 1 ]; then
        echo "✓ $name"; pass=$((pass+1))
    else
        echo "✗ $name —— 期望[$want]需含「$needle」，实际："
        printf '%s\n' "$out" | sed 's/^/    /'
        fail=$((fail+1))
    fi
}

# 一份两行的定义，C 段和 D 段反复要用
OPDEF='def <+> a b = a * 100 + b;\n'

echo "解释器：$EXE"
echo
echo "A. 声明与注册"

chk "def <+> 裸符号名 + infixr 声明" accept 'Value: 102' \
    "${OPDEF}infixr 5 <+>;\n1 <+> 2;\n"
chk "只 def 不声明：默认 infixl 9" accept 'Value: 102' \
    "${OPDEF}1 <+> 2;\n"
chk "def (<+>) 括号写法" accept 'Value: 304' \
    'def (<+>) a b = a * 100 + b;\ninfixl 6 <+>;\n3 <+> 4;\n'
chk "声明写在 def 前面（顺序无关）" accept 'Value: 102' \
    'infixl 6 <+>;\n'"${OPDEF}"'1 <+> 2;\n'
chk "声明被回显成自己" accept 'infixr 5 <+>;' \
    "${OPDEF}infixr 5 <+>;\n"
chk "infix（非结合）也回显" accept 'infix 4 <+>;' \
    "${OPDEF}infix 4 <+>;\n"
chk ":t 认得操作符表达式" accept 'Type : Int' \
    "${OPDEF}:t 1 <+> 2;\n"
chk "操作符在 def 体里可用" accept 'Value: 201' \
    "${OPDEF}def plusOne x = x <+> 1;\nplusOne 2;\n"
chk "操作符跨行注册（第三行才用）" accept 'Value: 102' \
    "${OPDEF}infixr 5 <+>;\n1 <+> 2;\n"
# 操作符就是个普通函数值，可以当参数传（没有段，用 lambda 顶）
chk "当高阶参数传（lambda 顶替段）" accept 'Value: 102' \
    "${OPDEF}def apply2 f a b = f a b;\napply2 (lambda x y -> x <+> y) 1 2;\n"

echo
echo "B. 优先级与结合性"

# ★ 声明真的在动优先级：同一个表达式，只有声明不同
chk "infixl 9 紧过 +" accept 'Value: 24' \
    'def <+> a b = a * 10 + b;\ninfixl 9 <+>;\n1 + 2 <+> 3;\n'
chk "infixl 1 松过 +" accept 'Value: 33' \
    'def <+> a b = a * 10 + b;\ninfixl 1 <+>;\n1 + 2 <+> 3;\n'
chk "infixr 6 右结合" accept 'Value: 8' \
    'def <+> a b = a - b;\ninfixr 6 <+>;\n10 <+> 3 <+> 1;\n'
chk "infixl 6 左结合" accept 'Value: 6' \
    'def <+> a b = a - b;\ninfixl 6 <+>;\n10 <+> 3 <+> 1;\n'
chk "重声明覆盖前面的" accept 'Value: 0' \
    'def <+> a b = a - b;\ninfixl 9 <+>;\ninfixl 1 <+>;\n1 + 2 <+> 3;\n'
# ★ 非结合：链式必须报错，而不是悄悄左结合
chk "infix 链式被拒" reject 'is non-associative' \
    'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> 2 <+> 3;\n'
chk "infix 单次没问题" accept 'Value: -1' \
    'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> 2;\n'
chk "infix 加括号就能连" accept 'Value: 2' \
    'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> (2 <+> 3);\n'
chk "不同操作符同层照样能连" accept 'Value: 3' \
    'def <+> a b = a - b;\ndef <*> a b = a + b;\ninfixl 6 <+>;\ninfixl 6 <*>;\n5 <+> 3 <*> 1;\n'

echo
echo "C. 该拒的"

# ★ 字符集政策：这些字符不许进操作符名，理由各不同
chk "拒绝 .（限定名要用）" reject 'is not an operator name' 'infixl 6 .;\n'
chk "拒绝 |（臂分隔符）"   reject 'is not an operator name' 'infixl 6 |;\n'
chk "拒绝 :（cons）"      reject 'is not an operator name' 'infixl 6 :;\n'
chk "拒绝 \$（编译器临时名）" reject 'is not an operator name' 'infixl 6 $;\n'
chk "拒绝 #（#cons 在用）" reject 'is not an operator name' 'infixl 6 #;\n'
# ★ 内建操作的优先级是 opInfo 写死的，改不了
chk "改内建 ++ 的 fixity 被拒" reject 'built-in operator' 'infixl 6 ++;\n'
chk "改内建 + 的 fixity 被拒"  reject 'built-in operator' 'infixr 1 +;\n'
chk "优先级必须是 0-9 一位" reject 'single digit 0-9' 'infixl 12 <+>;\n'
chk "优先级不是数字也拒"   reject 'single digit 0-9' 'infixl x <+>;\n'
chk "声明少写操作符也拒"   reject 'expected'          'infixl 6;\n'
# ★ 没声明就不是操作符：不许悄悄当普通函数调用（否则 `f x = 5` 这类手误会变成"未绑定变量"）
chk "没声明就不可用" reject 'Parse Error' '1 %% 2;\n'
# ★ 预扫描的行级判定：注释掉、放进字符串里都不算声明
chk "注释里的声明不算" reject 'Parse Error' '-- infixl 6 <+>;\n1 <+> 2;\n'
chk "字符串里的声明不算" accept 'Value: "infixl 6 <+>;"' \
    'def s = "infixl 6 <+>;";\ns;\n'
# ★ `string "infixl"` 没有 nextNotVar 保护时，这个变量名会被吃掉三个字符
chk "infixly 仍能做变量名" accept 'Value: 7' 'def infixly = 7;\ninfixly;\n'
chk "infix 前缀的变量名不受影响" accept 'Value: 3' 'def infx = 3;\ninfx;\n'

echo
echo "D. 回归（这些在打补丁前后都该一样）"

# ★ 重标 0..9 时把一元 - 的取数层从 3 挪到 8，这两条钉住挪对了
chk "-2 ^ 2 还是 -4" accept 'Value: -4' '-2 ^ 2;\n'
# ★ OpDiv 是 div（floor）：一元 - 若吃到 / 就会变成 -(7/2) = -3
chk "-7 / 2 还是 -4（floor）" accept 'Value: -4' '-7 / 2;\n'
chk "比较比 + 松" accept 'Value: true' '1 + 2 < 4;\n'
chk "== 比 + 松"   accept 'Value: true' '1 + 1 == 2;\n'
# ★ 一元 - 只吃内建：若它也吃用户操作符，这条会变成 -(1 <+> 2) = -102
chk "一元 - 不吃用户操作符" accept 'Value: -98' \
    "${OPDEF}-1 <+> 2;\n"
chk "二元 - 后面跟负数" accept 'Value: 3' '1 - -2;\n'
chk "++ 仍是 infixr 5" accept 'Value: "abc"' '"a" ++ "b" ++ "c";\n'
chk "++ 与用户操作符同层共存" accept 'Value: "ab"' \
    'def <+> a b = a ++ b;\ninfixr 5 <+>;\n"a" <+> "b";\n'

echo
echo "E. 接线（:load 进来的文件）"

cat > "$D/m.txt" <<'TXT'
def <+> a b = a * 100 + b;
infixl 6 <+>;
TXT
chk ":load 里的操作符回到 REPL 能用" accept 'Value: 102' \
    ":load $D/m.txt\n1 <+> 2;\n"
chk ":load 里的 def 也进来了" accept 'Type : Int' \
    ":load $D/m.txt\n:t 1 <+> 2;\n"

echo
if [ "$fail" -eq 0 ]; then
    echo "全对（$pass 条）"
    exit 0
fi
echo "$fail 条不对，见上。"
exit 1
