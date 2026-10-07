# scratch explorer #5 -- phantom constraints, unused implicit params, dedup
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
def a1 {o :: Ord a} x = x;
'@

Show @'
def a2 {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
'@

Show @'
def a2 {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
a2;
'@

Show @'
def a6 {o :: Ord a} x y = (o.cmp x y, 3);
'@

Show @'
def a7 {o :: Ord a} x = (o.cmp x x, o.cmp x x);
a7 3;
'@

Show @'
def f x = (min x x, max x x);
f 3;
'@

Show @'
implicit def base = Ord { cmp = lambda a b -> EQ } :: Ord Int;
implicit def derived = base;
min 1 2;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def outer x y = min x y;
def caller a b = outer a b;
caller 1 2;
caller "a" "b";
'@
