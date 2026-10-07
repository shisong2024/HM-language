# scratch explorer #10 -- self recursion, dedup across lambda, misc
$env:PSExecutionPolicyPreference = 'Bypass'
. (Join-Path $PSScriptRoot 'run.ps1') -Exe (Join-Path $PSScriptRoot 'v1.exe')
function Show {
    param([string]$Src)
    Write-Host '---------- program ----------'
    $Src -split "`n" | ForEach-Object { Write-Host "  |$_" }
    Write-Host '---------- output -----------'
    $out = Invoke-Interp -Source $Src
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "  >$_" }
}

Show @'
def count2 x = if true then x else min x (count2 x);
count2 5;
'@

Show @'
def count3 {o :: Ord a} x = if true then x else min x (count3 x);
count3 5;
'@

Show @'
def f x y = (min x y, lambda z -> min x z);
f 1 2;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def ev x = if x < 0 then odd9 x else min x 0;
def odd9 x = ev x;
ev 3;
'@

Show @'
def mkOrd f = Ord { cmp = f };
data Wrap a = Wrap a;
implicit def wInt = mkOrd (lambda a b -> EQ) :: Ord (Wrap Int);
def useW {o :: Ord (Wrap Int)} = o.cmp (Wrap 1) (Wrap 2);
useW;
'@
