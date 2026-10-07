#!/usr/bin/env bash
#
# Record（阶段 6）的探针。
#
# 用法：bash test/rev/probe-rec.sh test/rev/rec.exe    # 打了阶段 6 的影子
#       bash test/rev/probe-rec.sh test/rev/op.exe     # 对照：阶段 5，应大量全红
#
# 六类：
#   A. 声明与回显   —— `data ... = C { f :: T }`、REPL 回显带字段名、多字段、:t
#   B. 三种形态     —— `C { x = 1 }` 构造、`p.x` 取、`p { x = 3 }` 更新；
#                      字段顺序任意、可嵌套、可链式、位置构造子照旧能用
#   C. 模式         —— `C { x = p }`、只给部分字段、嵌套 record 模式、`_`
#   D. 该拒的       —— 字段重名（同声明 / 同类型跨构造子 / 跨声明）、未知字段、
#                      构造缺字段/多字段/重复给、非 record 构造子、更新混构造子、
#                      reserved 字段名、字段用在错的类型上（静态）
#   E. 运行时       —— 值为另一个构造子时 `p.x` 是运行期错误（静态类型相同，拦不住）；
#                      穷尽性检查没被绕过
#   F. 接线与回归   —— 跨模块 `import ... as M`（限定构造子、REPL 取字段、:t）、
#                      `:load`、REPL 里重声明 data 后旧字段失效
#
# 设计：脱糖在**整块解析完之后**做（`recProgram`），因为 `p.x` 可能出现在
# `data` 行之前，REPL 里也可能用的是上一行声明的类型。所以 D 段的报错走的是
# "Parse Error" 通道 —— 它确实是解析期发现的，只是晚了一步。
#
# 注：exe 是 Windows 二进制，stdin 必须重定向、输出是 CRLF。

set -uo pipefail
cd "$(dirname "$0")/../.." || exit 2

EXE="${1:-test/rev/rec.exe}"
[ -x "$EXE" ] || { echo "找不到 $EXE" >&2; exit 2; }

D=test/rev/recprobe
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

# 反复要用的声明
T2='data T = A | B { x :: Int, y :: Bool };\n'

# 跨模块用的模块（纯 ASCII，解释器输入必须是 ASCII）
cat > "$D/mod.txt" <<'EOF'
data Tree k v = Leaf | Node { h :: Int, key :: k, val :: v, l :: Tree k v, r :: Tree k v };
def mk k v = Node { h = 1, key = k, val = v, l = Leaf, r = Leaf };
def getKey t = t.key;
def bump t = t { h = 9 };
EOF

echo "解释器：$EXE"
echo
echo "A. 声明与回显"
chk "记录构造子声明"          accept 'B {x}'                 'data T = A | B { x :: Int };\n1;'
chk "多字段按声明序回显"      accept 'B {x, y}'              "$T2"'1;'
chk "单构造子记录类型"        accept 'W {w1}'                'data W = W { w1 :: Int };\n1;'
chk "普通构造子不受影响"      accept 'A : T'                 "$T2"'1;'
chk "多字段的 scheme"         accept 'Int -> (Bool -> T)'    "$T2"'1;'
chk ":t 取字段"               accept 'Type : Int'            "$T2"':t (B { x = 1, y = false }).x;'

echo
echo "B. 三种形态"
chk "构造（字段顺序任意）"    accept 'Value: (B 1 false)'    "$T2"'B { y = false, x = 1 };'
chk "位置构造子照旧能用"      accept 'Value: (B 1 false)'    "$T2"'B 1 false;'
chk "取字段"                  accept 'Value: 1'              "$T2"'(B { x = 1, y = false }).x;'
chk "取字段（先绑到 def）"    accept 'Value: 1'              "$T2"'def f t = t.x;\nf (B { x = 1, y = false });'
chk "更新只改点到的字段"      accept 'Value: (B 2 false)'    "$T2"'((B { x = 1, y = false }) { x = 2 });'
chk "更新全部字段"            accept 'Value: (B 2 true)'     "$T2"'((B { x = 1, y = false }) { x = 2, y = true });'
chk "更新后可读回旧字段"      accept 'Value: false'          "$T2"'(((B { x = 1, y = false }) { x = 2 }).y);'
chk "嵌套构造"                accept 'Value: (B 1 (B 2 A))'  'data L = A | B { x :: Int, l :: L };\nB { x = 1, l = B { x = 2, l = A } };'
chk "字段链式 .l.l"           accept 'Value: A'              'data L = A | B { x :: Int, l :: L };\n(B { x = 1, l = B { x = 2, l = A } }).l.l;'

echo
echo "C. 模式"
chk "record 模式"             accept 'Value: 1'              "$T2"'def f t = match t with B { x = y } -> y | A -> 0;\nf (B { x = 1, y = false });'
chk "只给部分字段"            accept 'Value: false'          "$T2"'def f t = match t with B { y = b } -> b | A -> true;\nf (B { x = 1, y = false });'
chk "字段给 _ 通配"           accept 'Value: 9'              "$T2"'def f t = match t with B { x = _ } -> 9 | A -> 0;\nf (B { x = 1, y = false });'
chk "嵌套 record 模式"        accept 'Value: 7'              'data L = A | B { x :: Int, l :: L };\ndef f t = match t with B { l = B { x = y } } -> y | B { l = A } -> 0 | A -> -1;\nf (B { x = 1, l = B { x = 7, l = A } });'
chk "record 模式里有字面量"   accept 'Value: (B 2 A)'        'data L = A | B { x :: Int, l :: L };\ndef f t = match t with B { x = 1, l = g } -> g | B { l = g } -> g | A -> A;\nf (B { x = 1, l = B { x = 2, l = A } });'
chk "括号里嵌 record 构造"    accept 'Value: (Just (B 3 true))' "$T2"'Just (B { x = 3, y = true });'
chk "record 模式与位置模式混" accept 'Value: (B 1 A)'        'data L = A | B { x :: Int, l :: L };\ndef f t = match t with B { x = 1, l = g } -> B { x = 1, l = g } | B { x = n, l = g } -> B { x = n + 1, l = g } | A -> A;\nf (B { x = 1, l = A });'

echo
echo "D. 该拒的"
chk "同一声明字段重名"        reject 'duplicate field'       'data D = D { a :: Int, a :: Bool };\n1;'
chk "同类型跨构造子重名"      reject 'duplicate field'       'data D = A1 { a :: Int } | A2 { a :: Bool };\n1;'
chk "跨声明字段重名"          reject 'already a field of'    'data D = D { a :: Int };\ndata E = E { a :: Bool };\n1;'
chk "未知字段"                reject 'unknown field'         "$T2"'def f t = t.nope;\n1;'
chk "构造时字段给多了"        reject 'is not a field of'     "$T2"'B { x = 1, y = true, nope = 3 };'
chk "构造时字段给少了"        reject 'still needs field'     "$T2"'B { x = 1 };'
chk "构造时字段给重了"        reject 'is given twice'        "$T2"'B { x = 1, x = 2, y = true };'
chk "非 record 构造子加字段"  reject 'is not a record constructor' "$T2"'A { x = 1 };'
chk "非 record 构造子上更新"  reject 'is not a record constructor' "$T2"'(A { x = 1 });'
chk "更新混两个构造子的字段"  reject 'cannot mix fields'     'data U = U1 { m :: Int } | U2 { n :: Int };\ndef g u = u { m = 1, n = 2 };\n1;'
chk "字段名是 reserved"       reject 'reserved symbol'       'data T = A | B { if :: Int };\n1;'
chk "模式里未知字段"          reject 'is not a field of'     "$T2"'def f t = match t with B { nope = n } -> n | A -> 0;\n1;'
chk "模式里字段给重了"        reject 'is given twice'        "$T2"'def f t = match t with B { x = n, x = m } -> n | A -> 0;\n1;'
chk "字段用在错的类型上"      reject 'Type mismatch'         "$T2"'data Q = Q;\ndef g q = q.x;\ng Q;'
chk "在元组上取字段"          reject 'unknown field'         '(1, 2).x;'

echo
echo "E. 运行时与穷尽性"
chk "值为另一个构造子"        reject 'is not a field of A'   "$T2"'def f t = t.x;\nf A;'
chk "取到正确构造子"          accept 'Value: 1'              "$T2"'def f t = t.x;\nf (B { x = 1, y = false });'
chk "单构造子无需兜底臂"      accept 'Value: 7'              'data W = W { w1 :: Int };\ndef f t = t.w1;\nf (W { w1 = 7 });'
chk "穷尽性没被绕过"          reject 'not exhaustive'        "$T2"'def f t = match t with B { x = y } -> y;'
chk "兜底臂算穷尽"            accept 'def f : (T -> Int)'    "$T2"'def f t = t.x;\n1;'

echo
echo "F. 接线与回归"
MOD="$D/mod.txt"
IMP="import $MOD as M;\n"
chk "跨模块调函数"            accept 'Value: 1'              "$IMP""M.getKey (M.mk 1 2);"
chk "跨模块 REPL 取字段"      accept 'Value: 1'              "$IMP""(M.mk 1 2).key;"
chk "跨模块限定构造子+字段"   accept 'Value: 1'              "$IMP""(M.Node { h = 5, key = 1, val = 2, l = M.Leaf, r = M.Leaf }).key;"
chk "跨模块 :t 取字段"        accept 'Type : Int'            "$IMP"":t (M.mk 1 2).key;"
chk ":load 后取字段"          accept 'Value: 1'              ":load $MOD\n(mk 1 2).key;"
chk ":load 后 :t 取字段"      accept 'Type : Int'            ":load $MOD\n:t (mk 1 2).key;"
chk "重声明 data 后旧字段没了" reject 'unknown field'        "$T2"'data T = A2 | B2 { y :: Bool };\ndef g t = t.x;\n1;'
chk "重声明 data 后新字段能用" accept 'Value: true'          "$T2"'data T = A2 | B2 { y :: Bool };\ndef g t = t.y;\ng (B2 { y = true });'
chk ":t 单选子函数"           accept '(T -> Int)'            "$T2"'def getX t = t.x;\n:t getX;'

echo
if [ "$fail" -eq 0 ]; then
    echo "全对（$pass 条）。"
else
    echo "$pass/$((pass+fail)) 条通过，$fail 条不对。"
fi
exit $(( fail > 0 ))
