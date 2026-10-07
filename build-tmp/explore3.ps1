# build-tmp/explore3.ps1 -- ASCII only. Round 3: cross-file (fixed paths), implicit, CRLF.
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

$root = (Get-Location).Path
$d = Join-Path $root 'build-tmp'
$enc = New-Object System.Text.UTF8Encoding($false)
[System.IO.File]::WriteAllText((Join-Path $d 'opmod1.txt'), "def <+> a b = a * 10 + b;`ninfixl 1 <+>;`n", $enc)
[System.IO.File]::WriteAllText((Join-Path $d 'opmod0.txt'), "def <+> a b = a * 10 + b;`n", $enc)
[System.IO.File]::WriteAllText((Join-Path $d 'opmod6.txt'), "def <+> a b = a * 10 + b;`ninfixr 6 <+>;`n", $enc)

p '68 two files: module fixity 1 applies'  'import build-tmp/opmod1.txt as M;\n1 + 2 <+> 3;\n'
p '69 two files: module default fixity'    'import build-tmp/opmod0.txt as M;\n1 + 2 <+> 3;\n'
p '70 two files: module op value + type'   'import build-tmp/opmod1.txt as M;\n1 <+> 2;\n(1 <+> 2) :: Int;\n'
p '70b two files: right assoc from module' 'import build-tmp/opmod6.txt as M;\n1 <+> 2 <+> 3;\n'
p '71 qualified op use M.<+>'              'import build-tmp/opmod1.txt as M;\n1 M.<+> 2;\n'
p '72 module fixity overridden in main'    'import build-tmp/opmod1.txt as M;\ninfixl 6 <+>;\n1 + 2 <+> 3;\n'
p '73 main def same op as module'          'import build-tmp/opmod1.txt as M;\ndef <+> a b = a + b;\n1 + 2 <+> 3;\n'
p '95 implicit def op no decl'             'implicit def <+> a b = a + b;\n1 <+> 2;\n'
p '96 implicit def op with decl'           'implicit def <+> a b = a + b;\ninfixl 6 <+>;\n1 <+> 2;\n'
p '97 def * builtin ignored'               'def * a b = a;\n2 * 3;\n'
p '98 def ++ self recursion'               'def ++ a b = a ++ b;\n"a" ++ "b";\n'
p '99 def ++ still fixity 5 r'             'def ++ a b = "x";\ninfixl 6 ++;\n"a" ++ "b" ++ "c";\n'
p '100 one-line two decls a*10+b'          'def <+> a b = a * 10 + b; infixl 1 <+>; infixl 9 <+>;\n1 + 2 <+> 3;\n'
p '101 no-space infix decl'                'def <+> a b = a + b;\ninfixl6 <+>;\n'
p '102 decl missing semi then use'         'def <+> a b = a + b;\ninfixl 6 <+>\n1 <+> 2;\n'
p '103 level plus sign'                    'def <+> a b = a + b;\ninfixl +6 <+>;\n'
p '104 op <+> with tab decl'               "def <+> a b = a + b;`n`tinfixl 6 <+>;`n1 <+> 2;`n"
p '105 level 0 vs cons'                    'def <+> a b = a * 100 + b;\ninfixl 0 <+>;\n1 : [2];\n'

# CRLF files run directly through the exe
$f1 = Join-Path $d 'crlf1.txt'
[System.IO.File]::WriteAllText($f1, "def <+> a b = a * 100 + b;`r`ninfixl 6 <+>;`r`n1 <+> 2;`r`n", $enc)
Write-Host '===== 106 CRLF decl on own line'
((& $Exe $f1) -join "`n" -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "   |$_" }

$f2 = Join-Path $d 'crlf2.txt'
[System.IO.File]::WriteAllText($f2, "def <+> a b = a * 10 + b; infixl 1 <+>;`r`n1 + 2 <+> 3;`r`n", $enc)
Write-Host '===== 107 CRLF decl mid-line'
((& $Exe $f2) -join "`n" -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "   |$_" }

$f3 = Join-Path $d 'crlf3.txt'
[System.IO.File]::WriteAllText($f3, "def <+> a b = a * 100 + b;`r`ninfixl 6 <+>;`r`n1 <+> 2;`r`n", $enc)
Write-Host '===== 108 CRLF def first no decl'
((& $Exe $f3) -join "`n" -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "   |$_" }
Show-Summary
