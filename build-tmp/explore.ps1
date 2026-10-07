# build-tmp/explore.ps1 -- ASCII only. Adversarial exploration for op fixity.
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

$E = '<BS>'
$OP = 'def <+> a b = a * 100 + b;\n'

p '01 oneliner decl ignored (want 33)'     'def <+> a b = a * 10 + b; infixl 1 <+>; 1 + 2 <+> 3;\n'
p '02 oneliner builtin decl accepted'      'def <+> a b = a + b; infixr 1 +;\n1 + 2 * 3;\n'
p '03 oneliner level 12 accepted'          'def <+> a b = a + b; infixl 12 <+>;\n1 <+> 2;\n'
p '04 def builtin + ignored'               'def + a b = a - b;\n1 + 2;\n'
p '05 def builtin ++ overridden'           'def ++ a b = 7;\n"a" ++ "b";\n'
p '06 nonassoc diff prec (report ok=2)'    'def <+> a b = a - b;\ndef <@> a b = a + b;\ninfix 6 <+>;\ninfix 2 <@>;\n1 <+> 2 <@> 3;\n'
p '07 nonassoc N then L same level'        'def <+> a b = a - b;\ndef <*> a b = a + b;\ninfix 6 <+>;\ninfixl 6 <*>;\n1 <+> 2 <*> 3;\n'
p '08 R then L same level'                 'def <+> a b = a - b;\ndef <*> a b = a + b;\ninfixr 6 <+>;\ninfixl 6 <*>;\n10 <+> 3 <*> 1;\n'
p '09 def paren with spaces'               'def ( <+> ) a b = a * 100 + b;\n1 <+> 2;\n'
p '10 def name on next line'               'def\n  <+> a b = a * 100 + b;\n1 <+> 2;\n'
p '11 = as operator name'                  'def = a b = a * 100 + b;\ninfixl 6 =;\n1 = 2;\n'
p '12 -> as operator name'                 'def -> a b = a * 100 + b;\ninfixl 5 ->;\n1 -> 2;\n'
p '13 -> does not break lambda'            'def -> a b = a * 100 + b;\ninfixl 5 ->;\n(lambda x -> x + 1) 2;\n'
p '14 section (<+>)'                       $OP'infixl 6 <+>;\n(<+>);\n'
p '15 section (<+> 1)'                     $OP'infixl 6 <+>;\n(<+> 1);\n'
p '16 section (1 <+>)'                     $OP'infixl 6 <+>;\n(1 <+>);\n'
p '17 op <+ prefix rules'                  'def <+ a b = a * 10 + b;\ndef <+> a b = a * 100 + b;\ninfixl 6 <+>;\ninfixl 6 <+>;\n1 <+ 2;\n3 <+> 4;\n'
p '18 op <== vs builtin <='                'def <== a b = a * 100 + b;\ninfixl 6 <==;\n3 <== 4;\n'
p '19 prec 8 vs ^'                         'def <+> a b = a + b;\ninfixl 8 <+>;\n2 ^ 3 <+> 4;\n'
p '20 prec 7 vs ^'                         'def <+> a b = a + b;\ninfixl 7 <+>;\n2 ^ 3 <+> 4;\n'
p '21 prec 9 vs ^'                         'def <+> a b = a + b;\ninfixl 9 <+>;\n2 ^ 3 <+> 4;\n'
p '22 unary minus prec9 user op'           'def <+> a b = a - b;\ninfixl 9 <+>;\n- 2 <+> 3;\n'
p '23 unary minus default'                 $OP'-1 <+> 2;\n'
p '24 rhs negative literal'                $OP'infixl 6 <+>;\n1 <+> -2;\n'
p '25 let/match/lambda/if'                 $OP'infixl 6 <+>;\nlet x = 1 <+> 2 in x;\n'
p '26 match arm'                           $OP'infixl 6 <+>;\nmatch 1 with | x -> x <+> 2;\n'
p '27 lambda body'                         $OP'infixl 6 <+>;\n(lambda x -> x <+> 1) 2;\n'
p '28 if branches'                         $OP'infixl 6 <+>;\nif true then 1 <+> 2 else 0;\n'
p '29 decl only, no def'                   'infixl 6 <+>;\n'
p '30 decl only then use'                  'infixl 6 <+>;\n1 <+> 2;\n'
p '31 infixl 0'                            'def <+> a b = a - b;\ninfixl 0 <+>;\n1 + 2 <+> 3;\n'
p '32 infixr 0 chain'                      'def <+> a b = a - b;\ninfixr 0 <+>;\n10 <+> 3 <+> 1;\n'
p '33 infixl 9 chain'                      'def <+> a b = a - b;\ninfixl 9 <+>;\n1 <+> 2 <+> 3;\n'
p '34 redef def, last wins'                'def <+> a b = a - b;\ndef <+> a b = a + b;\ninfixl 6 <+>;\n3 <+> 4;\n'
p '35 string stmt not a decl'              '"infixl 6 <+>;";\n1 <+> 2;\n'
p '36 string in def body'                  'def s = "infixl 6 <+>;";\ns;\n'
p '37 comment decl ignored'                '-- infixl 6 <+>;\n1 <+> 2;\n'
p '38 hof lambda'                          $OP'def apply2 f a b = f a b;\napply2 (lambda x y -> x <+> y) 1 2;\n'
p '39 both sides negative'                 $OP'infixl 6 <+>;\n-1 <+> -2;\n'
p '40 neg operand after *'                 'def <+> a b = a * 100 + b;\ndef <*> a b = a * b;\ninfixl 9 <+>;\n2 * -1 <+> 3;\n'
p '41 string ++ then user op'              'def <+> a b = b ++ a;\ninfixl 5 <+>;\n"a" ++ "b" <+> "c";\n'
p '42 string ++ then user op default'      'def <+> a b = b ++ a;\n"a" ++ "b" <+> "c";\n'
p '43 user op left of ++'                  'def <+> a b = b ++ a;\ninfixl 5 <+>;\n"a" <+> "b" ++ "c";\n'
p '44 records'                             'data P = P { px :: Int };\ndef <+> a b = P { px = a.px + b.px };\ninfixl 6 <+>;\n(P { px = 1 }) <+> (P { px = 2 });\n'
p '45 chain 3 levels'                      'def <+> a b = a * 10 + b;\ndef <@> a b = a * 100 + b;\ninfixl 6 <+>;\ninfixl 2 <@>;\n1 <@> 2 <+> 3;\n'
p '46 op +++ '                             'def +++ a b = a * 100 + b;\ninfixl 6 +++;\n1 +++ 2;\n'
p '47 mixed ++ and int'                    $OP'infixl 6 <+>;\n1 ++ "a" <+> 2;\n'
p '48 user op prec5 with builtin +'        'def <+> a b = a * 100 + b;\ninfixl 5 <+>;\n1 + 2 <+> 3;\n'
p '49 same name decl twice'                'def <+> a b = a - b;\ninfixl 9 <+>;\ninfixl 1 <+>;\n1 + 2 <+> 3;\n'
p '50 decl after use, same file'           '1 <+> 2;\ndef <+> a b = a * 100 + b;\n'
Show-Summary
