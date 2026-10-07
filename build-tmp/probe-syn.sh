#!/usr/bin/env bash
#
# 类型同义词（阶段 3）的探针。
#
# 用法：bash test/rev/probe-syn.sh test/rev/syn.exe     # 打了同义词补丁的影子
#       bash test/rev/probe-syn.sh test/rev/cur8.exe    # 对照：没做同义词，应全红
#
# 三类：
#   A. 该收的 —— 展开对不对（基本/跨批/前向引用/嵌套/参数交换/零参/多层）
#   B. 该拒的 —— 元数、环、保留名、重复参数、指向不存在的类型
#   C. 接线   —— data 里用同义词、`:t` 也要展开、跨模块加前缀、R3 重载
#
# ⚠️ 每条断言都在**没打补丁的 exe**（test/rev/cur8.exe）上确认过是红的：
#    旧 exe 根本不认识 `type` 这个关键字，A 类全部解析错、B 类全部报错文案
#    对不上、C 类同理。红了才说明它在测东西。
#
# 注：exe 是 Windows 二进制，stdin 必须重定向、输出是 CRLF。

set -uo pipefail
cd "$(dirname "$0")/../.." || exit 2

EXE="${1:-test/rev/syn.exe}"
[ -x "$EXE" ] || { echo "找不到 $EXE" >&2; exit 2; }

D=test/rev/synprobe
rm -rf "$D"; mkdir -p "$D"
pass=0; fail=0

# chk <名字> <accept|reject> <必须含的文本> <输入>
chk () {
    local name="$1" want="$2" needle="$3" input="$4"
    local out
    out=$(printf '%b' "$input" | timeout 20 "$EXE" 2>&1 | tr -d '\r')
    local ok=1
    case "$want" in
        reject) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:' || ok=0 ;;
        accept) printf '%s' "$out" | grep -qE 'Type Error:|Parse Error:' && ok=0 ;;
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

echo "解释器：$EXE"
echo
echo "A. 该收的"

chk "基本展开"      accept 'Type : (Int, Int)' \
    'type Pair a = (a, a);\n(1, 2) :: Pair Int;\n'
chk "跨批（第 2 行用第 1 行声明）" accept 'Value: 5' \
    'type T = Int;\n5 :: T;\n'
# 顺序无关：Point 写在 Pair 前面也要能用 —— 这条钉住「表里存 raw RHS」的设计
chk "前向引用（顺序无关）" accept 'Type : (Int, Int)' \
    'type Point = Pair Int;\ntype Pair a = (a, a);\n(3, 4) :: Point;\n'
chk "嵌套展开"      accept '((Int, Int), (Int, Int))' \
    'type Pair a = (a, a);\ntype Grid a = Pair (Pair a);\n((1, 2), (3, 4)) :: Grid Int;\n'
# 单趟替换的关键正确性：两趟会把 Swap Int Bool 变成 (Int, Int)
chk "参数交换"      accept '(Bool, Int)' \
    'type Pair a b = (a, b);\ntype Swap a b = Pair b a;\n(true, 1) :: Swap Int Bool;\n'
chk "零参同义词"    accept 'Type : (Int, Int)' \
    'type Name = (Int, Int);\n(1, 2) :: Name;\n'
chk "多层别名"      accept 'Value: 1' \
    'type A = Int;\ntype B = A;\ntype C = B;\n1 :: C;\n'
chk "两个参数都对上" accept 'Type : (Int, Bool)' \
    'type Pair a b = (a, b);\n(1, true) :: Pair Int Bool;\n'
chk "声明回显自己是自己" accept 'type Pair a = (a0, a0)' \
    'type Pair a = (a, a);\n'

echo
echo "B. 该拒的"

chk "元数多给"      reject 'expects 1 argument(s) but got 2' \
    'type Pair a = (a, a);\n(1, 2, 3) :: Pair Int Bool;\n'
chk "元数少给（裸名）" reject 'expects 1 argument(s) but got 0' \
    'type Pair a = (a, a);\n(1, 2) :: Pair;\n'
chk "互相成环"      reject 'cyclic type synonym' \
    'type A = B;\ntype B = A;\n1 :: A;\n'
chk "自引用（藏在元组里）" reject 'cyclic type synonym' \
    'type A = (A, Int);\n1 :: A;\n'
# ⚠️ 只钉到「保留名被拒」这一段。上游 src 在 32cc3d9 里把 parseTypeDecl 的文案
#    改成了「built-in ... cannot redeclared」，和 parseData 的「built in ...
#    cannot be redeclared.」不再一致 —— 这句话是用户的 src，不是我的补丁；
#    探针跟 src 走，别把某一次的措辞焊死。
chk "保留类型名"    reject 'type name and cannot' \
    'type Int = X;\n'
chk "重复类型参数"  reject 'duplicate type parameter a in type P' \
    'type P a a = (a, a);\n'
# 惰性：声明时不查目标存不存在，用的时候才报 —— 这是设计，不是漏检
chk "指向不存在的类型（用时才报）" reject 'Unknown type: Nope' \
    'type A = Nope;\n1 :: A;\n'

echo
echo "C. 接线"

chk "data 构造子参数里用同义词" accept 'D : ((Int, Int) -> U)' \
    'type Pair a = (a, a);\ndata U = D (Pair Int);\nD (1, 2);\n'
chk ":t 也要展开"   accept 'Type : Int' \
    'type T = Int;\n:t 5 :: T;\n'

# 跨模块：模块里的同义词名字要加前缀，用的时候要能展开
printf 'type Pair a = (a, a);\ntype Name = (Int, Bool);\ndef mk x = (x, x) :: Pair Int;\n' > "$D/modc.txt"
printf 'import modc.txt as C;\n' > "$D/drv.txt"
out=$(printf ':load %s/drv.txt\nC.mk 7;\n(1, true) :: C.Name;\n:q\n' "$D" \
      | timeout 25 "$EXE" 2>&1 | tr -d '\r')
if printf '%s' "$out" | grep -qF 'Value: (7, 7)' && printf '%s' "$out" | grep -qF 'Type : (Int, Bool)'; then
    echo "✓ 跨模块：名字加前缀 + 展开"; pass=$((pass+1))
else
    echo "✗ 跨模块：名字加前缀 + 展开 —— 实际："; printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi

# R3：改了模块里的同义词并重载，新定义必须生效（不是静默用旧的）
printf 'type T = Int;\ndef v = 1 :: T;\n' > "$D/modc.txt"
out=$({
    printf ':load %s/drv.txt\n' "$D"; sleep 1
    printf 'C.v;\n';                  sleep 1
    printf 'type T = Bool;\ndef v = true :: T;\n' > "$D/modc.txt"
    printf ':load %s/drv.txt\n' "$D"; sleep 1
    printf 'C.v;\n';                  sleep 1
    printf ':q\n'
} | timeout 60 "$EXE" 2>&1 | tr -d '\r')
got=$(printf '%s\n' "$out" | grep -E 'Value:|Type Error:' \
      | sed -E 's/^.*(Value:|Type Error:)/\1/' | tr '\n' '|')
if [ "$got" = 'Value: 1|Value: true|' ]; then
    echo "✓ R3：同义词改了要重载出新值"; pass=$((pass+1))
else
    echo "✗ R3：同义词改了要重载出新值 —— 期望 Value: 1|Value: true|，实际 ${got:-（空）}"
    printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi

echo
if [ "$fail" -eq 0 ]; then echo "全对（$pass 条）"; else echo "$fail 条不对，见上。"; fi
exit $(( fail > 0 ))
