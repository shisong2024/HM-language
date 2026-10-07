# build-tmp/probe-op.ps1 -- ASCII only.
#
# Port of test/rev/probe-op.sh (44 checks, the spec) to the PowerShell harness,
# plus adversarial checks for the custom-infix-operator + fixity feature.
#
# Run:
#   $env:PSExecutionPolicyPreference='Bypass'
#   cd E:\_love\interepter
#   & build-tmp\probe-op.ps1
#
# Porting notes (harness differences, NOT interpreter bugs):
#   * The .sh probe drives the REPL over stdin.  stdin piping is broken on this
#     machine, so every check runs the exe in FILE mode (`exe prog.txt`), which
#     parses the whole file as one batch.  Statements therefore must be newline
#     separated exactly as the .sh probe's \n strings already are.
#   * ":t <expr>" is a REPL-only command; in file mode it is ported to an
#     annotated expression `(expr) :: Int`, which exercises the same type check
#     and still prints "Type : Int".
#   * ":load f" is REPL-only; it is ported to an in-file `import f as M` line.
#     Both go through Interp.Main.loadWithImports, so the fixity-table wiring
#     under test is the same path; note that imports resolve relative to the
#     process working directory, hence the "build-tmp/..." prefix.
#   * probe-op.sh documents that operator sections / references ((<+>), (<+> 1),
#     (1 <+>)) are intentionally NOT implemented; those are checked as
#     documented rejections.

[CmdletBinding()]
param([string]$Exe = 'build-tmp/v1.exe')
$env:PSExecutionPolicyPreference = 'Bypass'
. (Join-Path $PSScriptRoot 'run.ps1') -Exe $Exe

# ---- module files used by the cross-file checks -----------------------------
$mdir = Join-Path (Get-Location).Path 'build-tmp'
$enc  = New-Object System.Text.UTF8Encoding($false)
[System.IO.File]::WriteAllText((Join-Path $mdir 'opmodA.txt'), "def <+> a b = a * 100 + b;`ninfixl 6 <+>;`n", $enc)
[System.IO.File]::WriteAllText((Join-Path $mdir 'opmod1.txt'), "def <+> a b = a * 10 + b;`ninfixl 1 <+>;`n", $enc)
[System.IO.File]::WriteAllText((Join-Path $mdir 'opmod0.txt'), "def <+> a b = a * 10 + b;`n", $enc)

# raw-file check (for CRLF, which Invoke-Interp cannot express)
function chkRaw {
    param([string]$Name, [string]$Want, [AllowEmptyString()][string]$Needle, [string]$File)
    $out = ((& $script:Exe $File 2>&1 | Out-String) -replace "`r", "")
    $ok = $true
    $hasErr = $out -match 'Type Error:|Parse Error:|Eval Error:'
    if ($Want -eq 'reject' -and -not $hasErr) { $ok = $false }
    if ($Want -eq 'accept' -and $hasErr)      { $ok = $false }
    if ($Needle -ne '' -and -not $out.Contains($Needle)) { $ok = $false }
    if ($ok) { Write-Host "  OK $Name"; $script:Pass++ }
    else {
        Write-Host "  FAIL $Name -- want[$Want] needle[$Needle]" -ForegroundColor Red
        ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "      | $_" }
        $script:Fail++; $script:Failures.Add($Name)
    }
}

$OPDEF = 'def <+> a b = a * 100 + b;\n'

Write-Host ''
Write-Host '=== PART 1: ported spec, group A (declaration/registration) ==='
chk 'A1  def <+> bare + infixr'          accept 'Value: 102' "${OPDEF}infixr 5 <+>;\n1 <+> 2;\n"
chk 'A2  def only: default infixl 9'     accept 'Value: 102' "${OPDEF}1 <+> 2;\n"
chk 'A3  def (<+>) paren form'           accept 'Value: 304' 'def (<+>) a b = a * 100 + b;\ninfixl 6 <+>;\n3 <+> 4;\n'
chk 'A4  decl before def'                accept 'Value: 102' "infixl 6 <+>;\n${OPDEF}1 <+> 2;\n"
chk 'A5  decl echoed back'               accept 'infixr 5 <+>;' "${OPDEF}infixr 5 <+>;\n"
chk 'A6  infix (non-assoc) echoed'       accept 'infix 4 <+>;' "${OPDEF}infix 4 <+>;\n"
chk 'A7  type of op expr (ex :t)'        accept 'Type : Int' "${OPDEF}infixl 6 <+>;\n(1 <+> 2) :: Int;\n"
chk 'A8  op usable in a def body'        accept 'Value: 201' "${OPDEF}def plusOne x = x <+> 1;\nplusOne 2;\n"
chk 'A9  op registered across lines'     accept 'Value: 102' "${OPDEF}infixr 5 <+>;\n1 <+> 2;\n"
chk 'A10 op passed as lambda HOF arg'    accept 'Value: 102' "${OPDEF}def apply2 f a b = f a b;\napply2 (lambda x y -> x <+> y) 1 2;\n"

Write-Host ''
Write-Host '=== PART 1: ported spec, group B (precedence/associativity) ==='
chk 'B1  infixl 9 binds tighter than +'  accept 'Value: 24' 'def <+> a b = a * 10 + b;\ninfixl 9 <+>;\n1 + 2 <+> 3;\n'
chk 'B2  infixl 1 looser than +'         accept 'Value: 33' 'def <+> a b = a * 10 + b;\ninfixl 1 <+>;\n1 + 2 <+> 3;\n'
chk 'B3  infixr 6 right assoc'           accept 'Value: 8'  'def <+> a b = a - b;\ninfixr 6 <+>;\n10 <+> 3 <+> 1;\n'
chk 'B4  infixl 6 left assoc'            accept 'Value: 6'  'def <+> a b = a - b;\ninfixl 6 <+>;\n10 <+> 3 <+> 1;\n'
chk 'B5  redeclaration overrides'        accept 'Value: 0'  'def <+> a b = a - b;\ninfixl 9 <+>;\ninfixl 1 <+>;\n1 + 2 <+> 3;\n'
chk 'B6  infix chain rejected'           reject 'is non-associative' 'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> 2 <+> 3;\n'
chk 'B7  infix single use fine'          accept 'Value: -1' 'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> 2;\n'
chk 'B8  infix with parens chains'       accept 'Value: 2'  'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> (2 <+> 3);\n'
chk 'B9  same-level distinct ops chain'  accept 'Value: 3'  'def <+> a b = a - b;\ndef <*> a b = a + b;\ninfixl 6 <+>;\ninfixl 6 <*>;\n5 <+> 3 <*> 1;\n'

Write-Host ''
Write-Host '=== PART 1: ported spec, group C (must be rejected) ==='
chk 'C1  reject . in op name'            reject 'is not an operator name' 'infixl 6 .;\n'
chk 'C2  reject | in op name'            reject 'is not an operator name' 'infixl 6 |;\n'
chk 'C3  reject : in op name'            reject 'is not an operator name' 'infixl 6 :;\n'
chk 'C4  reject $ in op name'            reject 'is not an operator name' 'infixl 6 $;\n'
chk 'C5  reject # in op name'            reject 'is not an operator name' 'infixl 6 #;\n'
chk 'C6  builtin ++ fixity rejected'     reject 'built-in operator' 'infixl 6 ++;\n'
chk 'C7  builtin + fixity rejected'      reject 'built-in operator' 'infixr 1 +;\n'
chk 'C8  two-digit precedence rejected'  reject 'single digit 0-9' 'infixl 12 <+>;\n'
chk 'C9  non-numeric precedence rejected' reject 'single digit 0-9' 'infixl x <+>;\n'
chk 'C10 missing op name rejected'       reject 'expected' 'infixl 6;\n'
chk 'C11 undeclared op rejected'         reject 'Parse Error' '1 %% 2;\n'
chk 'C12 commented decl is not a decl'   reject 'Parse Error' '-- infixl 6 <+>;\n1 <+> 2;\n'
chk 'C13 decl inside a string is not one' accept 'Value: "infixl 6 <+>;"' 'def s = "infixl 6 <+>;";\ns;\n'
chk 'C14 infixly still a variable name'  accept 'Value: 7' 'def infixly = 7;\ninfixly;\n'
chk 'C15 infix prefix var unaffected'    accept 'Value: 3' 'def infx = 3;\ninfx;\n'

Write-Host ''
Write-Host '=== PART 1: ported spec, group D (regressions) ==='
chk 'D1  -2 ^ 2 is still -4'             accept 'Value: -4' '-2 ^ 2;\n'
chk 'D2  -7 / 2 is still -4 (floor)'     accept 'Value: -4' '-7 / 2;\n'
chk 'D3  comparison looser than +'       accept 'Value: true' '1 + 2 < 4;\n'
chk 'D4  == looser than +'               accept 'Value: true' '1 + 1 == 2;\n'
chk 'D5  unary - does not eat user op'   accept 'Value: -98' "${OPDEF}-1 <+> 2;\n"
chk 'D6  binary - before negative'       accept 'Value: 3' '1 - -2;\n'
chk 'D7  ++ is still infixr 5'           accept 'Value: "abc"' '"a" ++ "b" ++ "c";\n'
chk 'D8  ++ coexists with user op at 5'  accept 'Value: "ab"' 'def <+> a b = a ++ b;\ninfixr 5 <+>;\n"a" <+> "b";\n'

Write-Host ''
Write-Host '=== PART 1: ported spec, group E (cross-file wiring; :load -> import) ==='
chk 'E1  op from loaded module usable'   accept 'Value: 102' 'import build-tmp/opmodA.txt as M;\n1 <+> 2;\n'
chk 'E2  op from module typechecks Int'  accept 'Type : Int' 'import build-tmp/opmodA.txt as M;\n(1 <+> 2) :: Int;\n'

Write-Host ''
Write-Host '=== PART 2: adversarial checks (X) ==='
Write-Host '-- X group 1: fixity declaration placement (scanFixities is line/word based)'
chk 'X1  one-line decl must apply (ex 33)'        accept 'Value: 33' 'def <+> a b = a * 10 + b; infixl 1 <+>; 1 + 2 <+> 3;\n'
chk 'X2  one-line builtin decl must be rejected'  reject 'built-in operator' 'def <+> a b = a + b; infixl 8 +;\n1 + 2 * 3;\n'
chk 'X3  one-line level-12 must be rejected'      reject 'single digit 0-9' 'def <+> a b = a + b; infixl 12 <+>;\n1 <+> 2;\n'
chk 'X4  one-line decl+def+use must parse'        accept 'Value: 33' 'infixl 1 <+>; def <+> a b = a * 10 + b;\n1 + 2 <+> 3;\n'
chk 'X5  two decls on one line are no-ops'        accept 'Value: 33' 'def <+> a b = a * 10 + b; infixl 1 <+>; infixl 9 <+>;\n1 + 2 <+> 3;\n'
chk 'X6  def<+> (no space) must register'         accept 'Value: 102' 'def<+> a b = a * 100 + b;\n1 <+> 2;\n'
chk 'X7  def ( <+> ) spaced parens must register' accept 'Value: 102' 'def ( <+> ) a b = a * 100 + b;\n1 <+> 2;\n'
chk 'X8  implicit def <+> needs default fixity'   accept 'Value: 3' 'implicit def <+> a b = a + b;\n1 <+> 2;\n'
chk 'X9  def name on next line must register'     accept 'Value: 102' 'def\n  <+> a b = a * 100 + b;\n1 <+> 2;\n'

Write-Host '-- X group 2: associativity / mixing'
chk 'X10 non-assoc, different precedences (ex 2)' accept 'Value: 2' 'def <+> a b = a - b;\ndef <%> a b = a + b;\ninfix 6 <+>;\ninfix 2 <%>;\n1 <+> 2 <%> 3;\n'
chk 'X11 non-assoc, higher first (ex -4)'         accept 'Value: -4' 'def <+> a b = a - b;\ndef <%> a b = a + b;\ninfix 2 <+>;\ninfix 6 <%>;\n1 <+> 2 <%> 3;\n'
chk 'X12 non-assoc same level must error'         reject 'is non-associative' 'def <+> a b = a - b;\ndef <%> a b = a + b;\ninfix 6 <+>;\ninfix 6 <%>;\n1 <+> 2 <%> 3;\n'
chk 'X13 infixr + infixl same level: cannot mix'  reject 'cannot mix' 'def <+> a b = a - b;\ndef <*> a b = a + b;\ninfixr 6 <+>;\ninfixl 6 <*>;\n10 <+> 3 <*> 1;\n'
chk 'X14 infix + infixl same level: cannot mix'   reject 'cannot mix' 'def <+> a b = a - b;\ndef <*> a b = a + b;\ninfix 6 <+>;\ninfixl 6 <*>;\n1 <+> 2 <*> 3;\n'
chk 'X15 infixl 8 vs builtin ^ (8) cannot mix'    reject 'cannot mix' 'def <+> a b = a + b;\ninfixl 8 <+>;\n2 ^ 3 <+> 4;\n'
chk 'X16 user infixl 5 vs builtin ++ (infixr 5)'  reject 'cannot mix' 'def <+> a b = b ++ a;\ninfixl 5 <+>;\n"a" ++ "b" <+> "c";\n'

Write-Host '-- X group 3: builtin operator names'
chk 'X17 def + must not be silently dead'         reject 'built-in operator' 'def + a b = a - b;\n1 + 2;\n'
chk 'X18 def * must not be silently dead'         reject 'built-in operator' 'def * a b = a;\n2 * 3;\n'
chk 'X19 def ++ must not hijack builtin ++'       reject 'built-in operator' 'def ++ a b = 7;\n"a" ++ "b";\n'
chk 'X20 builtin < fixity rejected (regression)'  reject 'built-in operator' 'infixl 6 <;\n'
chk 'X21 builtin == fixity rejected (regression)' reject 'built-in operator' 'infixl 6 ==;\n'
chk 'X22 builtin ^ fixity rejected (regression)'  reject 'built-in operator' 'infixr 9 ^;\n'

Write-Host '-- X group 4: sections (documented as unsupported) and op-as-value'
chk 'X23 (<+>) rejected (documented)'             reject 'Parse Error' "${OPDEF}infixl 6 <+>;\n(<+>);\n"
chk 'X24 (<+> 1) rejected (documented)'           reject 'Parse Error' "${OPDEF}infixl 6 <+>;\n(<+> 1);\n"
chk 'X25 (1 <+>) rejected (documented)'           reject 'Parse Error' "${OPDEF}infixl 6 <+>;\n(1 <+>);\n"
chk 'X26 op passed via lambda still works'        accept 'Value: 604' "${OPDEF}infixl 6 <+>;\n1 <+> (2 <+> (3 <+> 4));\n"

Write-Host '-- X group 5: precedence boundary / prefix interactions'
chk 'X27 op <+ (prefix of <+>) still parses'      accept 'Value: 12' 'def <+ a b = a * 10 + b;\ndef <+> a b = a * 100 + b;\ninfixl 6 <+>;\ninfixl 6 <+>;\n1 <+ 2;\n'
chk 'X28 <+> alongside <+'                      accept 'Value: 304' 'def <+ a b = a * 10 + b;\ndef <+> a b = a * 100 + b;\ninfixl 6 <+>;\ninfixl 6 <+>;\n3 <+> 4;\n'
chk 'X29 <== not confused with builtin <='        accept 'Value: 304' 'def <== a b = a * 100 + b;\ninfixl 6 <==;\n3 <== 4;\n'
chk 'X30 no-space application 1<+>2'              accept 'Value: 102' "${OPDEF}infixl 6 <+>;\n1<+>2;\n"
chk 'X31 operator at end of line'                 accept 'Value: 102' "${OPDEF}infixl 6 <+>;\n1 <+>\n2;\n"
chk 'X32 infixl 0 looser than everything'         accept 'Value: 0' 'def <+> a b = a - b;\ninfixl 0 <+>;\n1 + 2 <+> 3;\n'
chk 'X33 infixr 0 chain'                          accept 'Value: 8' 'def <+> a b = a - b;\ninfixr 0 <+>;\n10 <+> 3 <+> 1;\n'
chk 'X34 infixl 9 chain'                          accept 'Value: -4' 'def <+> a b = a - b;\ninfixl 9 <+>;\n1 <+> 2 <+> 3;\n'
chk 'X35 prec 9 tighter than builtin ^'           accept 'Value: 128' 'def <+> a b = a + b;\ninfixl 9 <+>;\n2 ^ 3 <+> 4;\n'
chk 'X36 prec 7 looser than builtin ^'            accept 'Value: 12' 'def <+> a b = a + b;\ninfixl 7 <+>;\n2 ^ 3 <+> 4;\n'
chk 'X37 prec 9 with unary-minus rhs'             accept 'Value: -196' "${OPDEF}infixl 9 <+>;\n1 + -2 <+> 3;\n"
chk 'X38 negative on both sides'                  accept 'Value: -102' "${OPDEF}infixl 6 <+>;\n-1 <+> -2;\n"
# NB: the harness only decodes \n and <BS>; a real tab must be a PS backtick-t.
chk 'X39 tab-indented decl'                       accept 'Value: 3' "def <+> a b = a + b;\n`tinfixl 6 <+>;\n1 <+> 2;\n"
chk 'X40 op name <-> (contains --) unusable'      reject 'must be at the start of a line' 'def <--> a b = a * 100 + b;\n1 <--> 2;\n'
chk 'X41 op name <- works'                        accept 'Value: 102' 'def <- a b = a * 100 + b;\ninfixl 6 <-;\n1 <- 2;\n'
chk 'X42 op name <- with negative rhs'            accept 'Value: 98' 'def <- a b = a * 100 + b;\ninfixl 6 <-;\n1 <- -2;\n'

Write-Host '-- X group 6: integration with other constructs'
chk 'X43 op over a record type'                   accept 'Value: (P 3)' 'data P = P { px :: Int };\ndef <+> a b = P { px = a.px + b.px };\ninfixl 6 <+>;\n(P { px = 1 }) <+> (P { px = 2 });\n'
chk 'X44 op inside let'                           accept 'Value: 102' "${OPDEF}infixl 6 <+>;\nlet x = 1 <+> 2 in x;\n"
chk 'X45 op inside a match arm'                   accept 'Value: 102' "${OPDEF}infixl 6 <+>;\nmatch 1 with x -> x <+> 2;\n"
chk 'X46 op inside a lambda body'                 accept 'Value: 201' "${OPDEF}infixl 6 <+>;\n(lambda x -> x <+> 1) 2;\n"
chk 'X47 op inside if branches'                   accept 'Value: 102' "${OPDEF}infixl 6 <+>;\nif true then 1 <+> 2 else 0;\n"
chk 'X48 op inside list literal'                  accept 'Value: [102, 304]' "${OPDEF}infixl 6 <+>;\n[1 <+> 2, 3 <+> 4];\n"
chk 'X49 op inside tuple'                         accept 'Value: (102, 3)' "${OPDEF}infixl 6 <+>;\n(1 <+> 2, 3);\n"
chk 'X50 annotation after user op'                accept 'Value: 102' "${OPDEF}infixl 6 <+>;\n1 <+> 2 :: Int;\n"
chk 'X51 = accepted as op name (policy gap)'      accept 'Value: 102' 'def = a b = a * 100 + b;\ninfixl 6 =;\n1 = 2;\n'
chk 'X52 -> accepted as op name (policy gap)'     accept 'Value: 102' 'def -> a b = a * 100 + b;\ninfixl 5 ->;\n1 -> 2;\n'
chk 'X53 -> does not break lambda/match'          accept 'Value: 2' 'def -> a b = a * 100 + b;\ninfixl 5 ->;\nmatch 1 with x -> x + 1;\n'
chk 'X54 = does not break let/record'             accept 'Value: 1' 'def = a b = a * 100 + b;\ninfixl 6 =;\nlet x = 1 in x;\n'
chk 'X55 op over strings keeps builtin ++ order'  accept 'Value: "acb"' 'def <+> a b = b ++ a;\n"a" ++ "b" <+> "c";\n'

Write-Host '-- X group 7: cross-file / qualification'
chk 'X56 module fixity 1 applied in importer'     accept 'Value: 33' 'import build-tmp/opmod1.txt as M;\n1 + 2 <+> 3;\n'
chk 'X57 module default fixity (9) in importer'   accept 'Value: 24' 'import build-tmp/opmod0.txt as M;\n1 + 2 <+> 3;\n'
chk 'X58 qualified op use M.<+> rejected'         reject 'Parse Error' 'import build-tmp/opmodA.txt as M;\n1 M.<+> 2;\n'
chk 'X59 op from module usable unqualified'       accept 'Value: 12' 'import build-tmp/opmod1.txt as M;\n1 <+> 2;\n'

Write-Host '-- X group 8: CRLF source (raw file)'
$crlf = Join-Path $mdir 'probeop-crlf.txt'
[System.IO.File]::WriteAllText($crlf, "def <+> a b = a * 100 + b;`r`ninfixl 6 <+>;`r`n1 <+> 2;`r`n", $enc)
chkRaw 'X60 CRLF decl on its own line applies' 'accept' 'Value: 102' $crlf
[System.IO.File]::WriteAllText($crlf, "def <+> a b = a * 10 + b; infixl 1 <+>;`r`n1 + 2 <+> 3;`r`n", $enc)
chkRaw 'X61 CRLF mid-line decl must apply' 'accept' 'Value: 33' $crlf

Write-Host ''
Show-Summary | Out-Null
Write-Host ''
Write-Host "TOTAL: $($script:Pass) passed, $($script:Fail) failed"
if ($script:Fail -gt 0) {
    Write-Host 'FAILED CHECKS:'
    $script:Failures | ForEach-Object { Write-Host "  - $_" }
}
