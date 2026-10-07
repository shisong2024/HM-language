# scratch explorer #9 -- known findings, last checks
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
def a {o :: Ord a} = o;
(a :: Ord Int);
'@

Show @'
def a2b {o :: Ord a} x = (o, x, Ord { cmp = lambda p q -> EQ } :: Ord [a]);
'@

Show @'
def a2c {o :: Ord a} x = Ord { cmp = lambda p q -> EQ } :: Ord [a];
'@

Show @'
data Color = Red | Blue;
def mkOrd f = Ord { cmp = f };
implicit def badColor = 3 :: Ord Color;
def useC {o :: Ord Color} x = o.cmp x x;
useC Red;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def outer x = let g = lambda y -> min x y in g x;
outer 3;
'@

Show @'
implicit def badC {o :: Ord a} = o;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit loc = (mkOrd (lambda a b -> EQ) :: Ord Int) in (loc.cmp 5 7, x);
f 1;
'@
