# scratch explorer #8 -- nearest frame details, multi-dict order, spans
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
def mkOrd f = Ord { cmp = f };
def f x = let implicit loc = (mkOrd (lambda a b -> GT) :: Ord Int) in (min 5 7, x);
f 1;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit s = (mkOrd (lambda a b -> GT) :: Ord String) in (min 5 7, x);
f 1;
'@

Show @'
def mkOrd f = Ord { cmp = f };
let implicit i1 = (mkOrd (lambda a b -> GT) :: Ord Int) in (let implicit i2 = (mkOrd (lambda a b -> LT) :: Ord Int) in min 5 7);
'@

Show @'
def g {o :: Ord a} {p :: Ord b} x y = (o.cmp x x, p.cmp y y);
g 1 "a";
'@

Show @'
def mkOrd f = Ord { cmp = f };
data Color = Red | Blue;
implicit def c1 = mkOrd (lambda a b -> EQ) :: Ord Color;
def wrap x = (1, min x x);
wrap Red;
'@

Show @'
def mkOrd f = Ord { cmp = f };
implicit def a1 = mkOrd (lambda a b -> EQ) :: Ord Int;
implicit def a2 = mkOrd (lambda a b -> EQ) :: Ord Int;
def deep z =
    let y = z in
    min y 0;
deep 5;
'@
