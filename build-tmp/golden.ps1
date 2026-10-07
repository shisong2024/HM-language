# build-tmp/golden.ps1 -- PowerShell equivalent of test/run-golden.sh
# Run:  $env:PSExecutionPolicyPreference='Bypass'; & build-tmp\golden.ps1 -Exe build-tmp\v1.exe
# NOTE: keep this file ASCII-only -- PowerShell reads .ps1 as ANSI/GBK on this machine,
#       so non-ASCII text in a BOM-less file gets mangled into parse errors.
[CmdletBinding()]
param([string]$Exe = 'build-tmp\v1.exe')
$ErrorActionPreference = 'Continue'
Set-Location (Split-Path -Parent $PSScriptRoot)

. build-tmp\run.ps1 -Exe $Exe

Write-Host ''
Write-Host 'CHECK 1: REPL does not hang'
$r1 = Invoke-Interp -Source "1+1;\n"
if ($r1 -match '<<TIMEOUT>>') { Write-Host '  X timeout' -ForegroundColor Red; $script:Fail++ }
else { Write-Host '  OK exits normally'; $script:Pass++ }

Write-Host 'CHECK 2: runaway recursion is caught by the depth guard'
$r2 = Invoke-Interp -Source "def f n = if n == 0 then 1 else f n;\nf 1;\n"
if ($r2 -match '<<TIMEOUT>>') { Write-Host '  X timeout (memory bomb)' -ForegroundColor Red; $script:Fail++ }
elseif ($r2 -match 'Recursion Limited') { Write-Host '  OK Recursion Limited'; $script:Pass++ }
else { Write-Host "  X unexpected: $r2" -ForegroundColor Red; $script:Fail++ }

Write-Host ''
Write-Host 'GOLDEN CORPUS'
chk_file 'baseline test.txt'        'test.txt'              'test/expected.txt'              | Out-Null
chk_file 'parse errors'             'test/parse-errors.txt' 'test/parse-errors.expected.txt' | Out-Null
chk_file 'known-bugs (empty)'       'test/known-bugs.txt'   'test/known-bugs.current.txt'    | Out-Null

Write-Host ''
Write-Host 'REPL CHECKS'
chk 'REPL evaluates last line'      accept 'Value: 2'   "1+1;\n"
chk 'REPL keeps defs across lines'  accept 'Value: 11'  "def f x = x + 1;\nf 10;\n"
chk 'REPL 0-arg def'                accept 'Value: 1'   "def x = 1;\nx;\n"
chk 'REPL :t expression'            accept 'Type : Int' ":t 1+1\n"
chk 'REPL :t with trailing semi'    accept 'Type : Int' ":t 1+1;\n"
chk 'REPL :t sees session defs'     accept 'Int -> Int' "def f x = x + 1;\n:t f\n"
chk 'REPL parse error does not exit' accept 'Value: 2'  "(((\n1+1;\n"
chk 'REPL type vars start at a0'    accept 'a0 -> a0'   "lambda a -> a;\nlambda b -> b;\n"
chk 'long session :t restarts at a0' accept '((a0 -> a1) -> ([a0] -> [a1]))' ":load utils/prelude.txt\n:t map\n"
chk 'REPL def body calls prior def' accept 'Value: 2'   "def f x = x + 1;\ndef g x = f x;\ng 1;\n"
chk 'REPL max2/max3 session'        accept 'Value: 4'   "def max2 a b = if a < b then b else a;\ndef max3 a b c = if max2 a b == a then if max2 a c == a then a else c else if max2 b c == b then b else c;\nmax3 1 4 3;\n"

exit ([int](-not (Show-Summary)))
