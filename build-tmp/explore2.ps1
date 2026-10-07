# build-tmp/explore2.ps1 -- ASCII only. Round 2: corrected + cross-file.
[CmdletBinding()]
param([string]$Exe = 'build-tmp/v1.exe')
$env:PSExecutionPolicyPreference = 'Bypass'
. (Join-Path $PSScriptRoot 'run.ps1') -Exe $Exe

function p {
    param([string]$Name, [string]$Src)
    $out = Invoke-Interp -Source $Src
    Write-Host "===== $Name"
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "   |$_" }
}

$OP = 'def <+> a b = a * 100 + b;\n'

# ---- module files for cross-file tests (written under build-tmp) ----
$d = Join-Path (Get-Location).Path 'build-tmp'
[System.IO.File]::WriteAllText((Join-Path $d 'opmod1.txt'), "def <+> a b = a * 10 + b;`ninfixl 1 <+>;`n", (New-Object System.Text.UTF8Encoding($false)))
[System.IO.File]::WriteAllText((Join-Path $d 'opmod0.txt'), "def <+> a b = a * 10 + b;`n", (New-Object System.Text.UTF8Encoding($false)))

p '06b nonassoc higher then lower (report ok)' 'def <+> a b = a - b;\ndef <%> a b = a + b;\ninfix 6 <+>;\ninfix 2 <%>;\n1 <+> 2 <%> 3;\n'
p '06c nonassoc lower then higher'             'def <+> a b = a - b;\ndef <%> a b = a + b;\ninfix 2 <+>;\ninfix 6 <%>;\n1 <+> 2 <%> 3;\n'
p '06d same level two N ops'                   'def <+> a b = a - b;\ndef <%> a b = a + b;\ninfix 6 <+>;\ninfix 6 <%>;\n1 <+> 2 <%> 3;\n'
p '26b match arm (fixed syntax)'               $OP'infixl 6 <+>;\nmatch 1 with x -> x <+> 2;\n'
p '51 oneliner builtin + shows no effect'      'def <+> a b = a + b; infixl 8 +;\n1 + 2 * 3;\n'
p '52 oneliner builtin ++ decl'                'def <+> a b = a + b; infixl 6 ++;\n"a" ++ "b";\n'
p '53 oneliner two decls'                      'def <+> a b = a + b; infixl 1 <+>; infixl 9 <+>;\n1 + 2 <+> 3;\n'
p '54 decl before def on one line'             'infixl 1 <+>; def <+> a b = a * 10 + b;\n1 + 2 <+> 3;\n'
p '55 def<+> no space'                         'def<+> a b = a * 100 + b;\n1 <+> 2;\n'
p '56 paren-space def works with decl'         'def ( <+> ) a b = a * 100 + b;\ninfixl 6 <+>;\n1 <+> 2;\n'
p '57 = op then let/record/match survive'      'def = a b = a * 100 + b;\ninfixl 6 =;\nlet x = 1 in x;\ndata P = P { px :: Int };\n(P { px = 1 }).px;\nmatch 1 with x -> x + 1;\n'
p '58 -> op then match survives'               'def -> a b = a * 100 + b;\ninfixl 5 ->;\nmatch 1 with x -> x + 1;\n'
p '59 nonassoc chain error msg'                'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> 2 <+> 3;\n'
p '60 nonassoc paren both sides'               'def <+> a b = a - b;\ninfix 6 <+>;\n(1 <+> 2) <+> 3;\n'
p '61 nonassoc 4 chain'                        'def <+> a b = a - b;\ninfix 6 <+>;\n1 <+> 2 <+> 3 <+> 4;\n'
p '62 builtin < fixity decl rejected'          'infixl 6 <;\n'
p '63 builtin == fixity decl rejected'         'infixl 6 ==;\n'
p '64 builtin ^ fixity decl rejected'          'infixr 9 ^;\n'
p '65 builtin < on one line'                   'def <+> a b = a + b; infixl 6 <;\n1 < 2;\n'
p '66 default chain 3'                         $OP'1 <+> 2 <+> 3;\n'
p '67 user op prec1 vs + left'                 'def <+> a b = a - b;\ninfixl 1 <+>;\n1 + 2 <+> 3;\n'
p '68 two files: module fixity 1 applies'      'import opmod1.txt as M;\n1 + 2 <+> 3;\n'
p '69 two files: module default fixity'        'import opmod0.txt as M;\n1 + 2 <+> 3;\n'
p '70 two files: module op value + type'       'import opmod1.txt as M;\n1 <+> 2;\n(1 <+> 2) :: Int;\n'
p '71 qualified op use M.<+>'                  'import opmod1.txt as M;\n1 M.<+> 2;\n'
p '72 module fixity overridden in main'        'import opmod1.txt as M;\ninfixl 6 <+>;\n1 + 2 <+> 3;\n'
p '73 main def same op as module'              'import opmod1.txt as M;\ndef <+> a b = a + b;\n1 <+> 2;\n'
p '74 infixly / infx still variables'          'def infixly = 7;\ndef infx = 3;\ninfixly;\ninfx;\n'
p '75 undeclared op rejected'                  '1 %% 2;\n'
p '76 regressions -2^2 and -7/2'               '-2 ^ 2;\n-7 / 2;\n'
p '77 ++ is infixr 5'                          '"a" ++ "b" ++ "c";\n'
p '78 unary minus not eating user op'          'def <+> a b = a * 100 + b;\n-1 <+> 2;\n'
p '79 infixl 9 oneliner vs own line'           'def <+> a b = a * 10 + b;\ninfixl 9 <+>;\n1 + 2 <+> 3;\n'
p '80 infixl 9 same line'                      'def <+> a b = a * 10 + b; infixl 9 <+>;\n1 + 2 <+> 3;\n'
p '81 op name with two dashes'                 'def <--> a b = a * 100 + b;\n1 <--> 2;\n'
p '82 op name lt-minus'                        'def <- a b = a * 100 + b;\ninfixl 6 <-;\n1 <- 2;\n'
p '83 op name lt-minus neg rhs'                'def <- a b = a * 100 + b;\ninfixl 6 <-;\n1 <- -2;\n'
p '84 op applied to lambda-returned value'     $OP'infixl 6 <+>;\n(lambda x -> x) 1 <+> 2;\n'
p '85 op in list literal'                      $OP'infixl 6 <+>;\n[1 <+> 2, 3 <+> 4];\n'
p '86 op in tuple'                             $OP'infixl 6 <+>;\n(1 <+> 2, 3);\n'
p '87 cons vs user op prec 0'                  $OP'infixl 0 <+>;\n1 : [2];\n'
p '88 annotation after user op'                $OP'infixl 6 <+>;\n1 <+> 2 :: Int;\n'
p '89 nested parens deep'                      $OP'infixl 6 <+>;\n1 <+> (2 <+> (3 <+> 4));\n'
p '90 prec9 user op after *'                   $OP'infixl 9 <+>;\n2 * 1 <+> 3;\n'
p '91 prec9 user op with unary minus rhs'      $OP'infixl 9 <+>;\n1 + -2 <+> 3;\n'
p '92 decl level 9 then minus operand'         $OP'infixl 9 <+>;\n-2 <+> 3;\n'
p '93 decl level 8 then minus operand'         'def <+> a b = a - b;\ninfixl 8 <+>;\n-2 <+> 3;\n'
p '94 decl level 8 vs builtin ^ operand'       'def <+> a b = a - b;\ninfixl 8 <+>;\n-2 ^ 2;\n'
Show-Summary
