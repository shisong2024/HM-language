#!/usr/bin/env bash
#
# 字符串（阶段 4）的探针。
#
# 用法：bash test/rev/probe-str.sh test/rev/str.exe        # 打了字符串补丁的影子
#       bash test/rev/probe-str.sh test/rev/pre-str.exe    # 对照：没做字符串，应全红
#
# 四类：
#   A. 字面量与值   —— 回显、转义、\uXXXX、uncons/fromCode、字面量模式、`--` 感知
#   B. 该拒的       —— String 与 Int/[Int] 不通用、码点越界、保留类型名、没有 <
#   C. 预置库       —— prelude.txt 里靠那 3 个原语写出来的整套函数
#   D. 接线         —— :: String 注解、data 参数、跨模块（uncons 结果要跟 prelude 的 Maybe 合一）
#
# ⚠️ 每条断言都在**没打补丁的 exe**（test/rev/pre-str.exe，从当时的 src 现编的）上
#    确认过是红的：A 类全部解析错（字符串字面量根本不认识）、B 类报错文案对不上、
#    C 类全是 "is an unbound variable"、D 类同理。红了才说明它在测东西。
#
# 注：exe 是 Windows 二进制，stdin 必须重定向、输出是 CRLF。

set -uo pipefail
cd "$(dirname "$0")/../.." || exit 2

EXE="${1:-test/rev/str.exe}"
[ -x "$EXE" ] || { echo "找不到 $EXE" >&2; exit 2; }

D=test/rev/strprobe
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

echo "解释器：$EXE"
echo
echo "A. 字面量与值"

chk "字面量回显 + 类型" accept 'Type : String' '"abc";\n'
chk "++ 拼接（右结合，一次跑完）" accept 'Value: "abc"' '"a" ++ "b" ++ "c";\n'
chk "== 相等"  accept 'Value: true'  '"a" == "a";\n'
chk "!= 不等"  accept 'Value: true'  '"a" != "b";\n'
chk "比的是内容不是位置" accept 'Value: true' '("ab" ++ "c") == ("a" ++ "bc");\n'
# 回显必须是可读回来的字面量形式：\t \r \\ \" 走短转义
chk "转义回显 \\t"  accept 'Value: "a\tb"'   '"a\\tb";\n'
chk "转义回显 \\r"  accept 'Value: "a\rb"'   '"a\\rb";\n'
chk "转义回显 \\\\" accept 'Value: "a\\b"' '"a\\\\b";\n'
chk "转义回显 \\\"" accept 'Value: "a\"b"'   '"a\\"b";\n'
# 源文件必须纯 ASCII —— \uXXXX 是写出非 ASCII 字符串的唯一途径
chk "\\uXXXX（ASCII 码点）"  accept 'Value: "A"' '"\\u0041";\n'
chk "\\uXXXX（非 ASCII 码点）" accept 'Value: "\u25B6"' '"\\u25B6";\n'
chk "uncons 有头" accept '(Just (97, "bc"))' 'uncons "abc";\n'
chk "uncons 空串" accept 'Value: None'       'uncons "";\n'
chk "fromCode"   accept 'Value: "A"'         'fromCode 65;\n'
chk "字面量模式命中" accept 'Value: 1' 'match "GET" with "GET" -> 1 | _ -> 2;\n'
chk "字面量模式落空" accept 'Value: 2' 'match "PUT" with "GET" -> 1 | _ -> 2;\n'
# ★ stripComment 是纯文本扫描、跑在解析之前：不认字符串就会把 `"a--b"` 截成 `"a`
chk "字符串里的 -- 不是注释" accept 'Value: "a--b"' '"a--b";\n'
chk "字符串里的 \\\" 不结束扫描" accept 'Value: "a\"--b"' '"a\\"--b";\n'

echo
echo "B. 该拒的"

# ★ unify 的兜底分支：漏掉 (TString, TString) 会变成「字符串连自己都合不上」
chk "String 不是 Int"   reject 'expected Int but got String'    '"a" + 1;\n'
chk "String 不是 [Int]" reject 'expected [Int] but got String'  '("abc" :: [Int]);\n'
chk "String 不能进列表" reject 'expected [Int] but got String'  'def g x = x ++ [1];\n'
chk "注解也要对上"      reject 'expected String but got Int'    '(5 :: String);\n'
# ★ 类型错的表达式照样会被求值（只是不报它的类型），于是原语会真的拿到错类型的
#   实参。兜底分支以前是 `error`，那会把整个进程连同那条类型错误一起带走。
#   这条钉住「报类型错、不崩、后面的语句照跑」——查 CallStack 是判崩的指纹。
out=$(printf 'strLen 5;\n1 + 1;\n:q\n' | timeout 20 "$EXE" 2>&1 | tr -d '\r')
if printf '%s' "$out" | grep -qF 'expected Int but got String' \
   && printf '%s' "$out" | grep -qF 'Value: 2' \
   && ! printf '%s' "$out" | grep -qF 'CallStack'; then
    echo "✓ 原语拿到错类型的实参：报错不崩、后面的语句照跑"; pass=$((pass+1))
else
    echo "✗ 原语拿到错类型的实参：报错不崩、后面的语句照跑 —— 实际："
    printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi
# 字符是 Int 码点，`<` 只认 Int —— 字符串没有字典序，这条钉住「不给它偷偷加」
chk "字符串没有 <"      reject 'expected Int but got String'    '"a" < "b";\n'
chk "模式类型要对上"    reject 'Type mismatch'                  'match 5 with "a" -> 1 | _ -> 2;\n'
chk "码点越界（大）"    reject 'Not a Unicode code point: 65536' 'fromCode 65536;\n'
chk "码点越界（负）"    reject 'Not a Unicode code point: -1'    'fromCode (0 - 1);\n'
chk "保留类型名（type）" reject 'type name and cannot' 'type String = Int;\n'
chk "保留类型名（data）" reject 'type name and cannot' 'data String = Q;\n'

echo
echo "C. 预置库（prelude.txt 里用那 3 个原语写出来的）"

chk "strLen"      accept 'Value: 5'          'strLen "Hello";\n'
chk "strReverse"  accept 'Value: "cba"'      'strReverse "abc";\n'
chk "strToUpper"  accept 'Value: "HELLO!"'   'strToUpper "Hello!";\n'
chk "strSplitAt"  accept '("abc", "def")'    'strSplitAt 3 "abcdef";\n'
chk "strWords"    accept '["the", "quick"]'  'strWords "  the quick ";\n'
chk "strLines 末尾换行不加行" accept 'Value: ["a", "b"]' 'strLines "a\\nb\\n";\n'
chk "showInt / readInt 往返" accept 'Value: (Just 123)' 'readInt (showInt 123);\n'
chk "readInt 非数字" accept 'Value: None'    'readInt "12x";\n'
chk "isDigit 是码点层" accept 'Value: true'  'isDigit 53;\n'

echo
echo "D. 接线"

chk ":: String 注解" accept 'Type : String' '("hi" :: String) ++ "!";\n'
chk ":t 认字符串"    accept 'Type : String' ':t "abc";\n'
chk "data 参数是 String" accept 'S : (String -> S)' 'data S = S String;\nS "q";\n'

# 跨模块：uncons 的返回类型写死在 Builtin 里是 `TCon "Maybe"`（不带前缀），
# 模块里的 Maybe 也不带前缀 —— 这条钉住「两边能合一」，否则模块里根本用不了 uncons
printf 'def firstChar s = match uncons s with None -> 0 | Just (c, _) -> c;\ndef shout s = match uncons s with None -> "" | Just (c, rest) -> fromCode (c - 32) ++ shout rest;\n' > "$D/mods.txt"
printf 'import mods.txt as M;\nM.firstChar "A";\nM.shout "abc";\n' > "$D/drvs.txt"
out=$(printf ':load %s/drvs.txt\nM.firstChar "A";\nM.shout "abc";\n:q\n' "$D" \
      | timeout 25 "$EXE" 2>&1 | tr -d '\r')
if printf '%s' "$out" | grep -qF 'Value: 65' && printf '%s' "$out" | grep -qF 'Value: "ABC"'; then
    echo "✓ 跨模块：uncons 与 prelude 的 Maybe 合一"; pass=$((pass+1))
else
    echo "✗ 跨模块：uncons 与 prelude 的 Maybe 合一 —— 实际："; printf '%s\n' "$out" | sed 's/^/    /'; fail=$((fail+1))
fi

echo
if [ "$fail" -eq 0 ]; then echo "全对（$pass 条）"; else echo "$fail 条不对，见上。"; fi
exit $(( fail > 0 ))
