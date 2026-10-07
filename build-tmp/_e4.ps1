# scratch explorer #4 -- module / session state, scope reset (fixed paths)
$env:PSExecutionPolicyPreference = 'Bypass'
. (Join-Path $PSScriptRoot 'run.ps1') -Exe (Join-Path $PSScriptRoot 'v1.exe')

$bt = $PSScriptRoot
function W([string]$n, [string]$t) { [System.IO.File]::WriteAllText((Join-Path $bt $n), $t, (New-Object System.Text.UTF8Encoding($false))) }
function Show {
    param([string]$Src)
    Write-Host '---------- program ----------'
    $Src -split "`n" | ForEach-Object { Write-Host "  |$_" }
    Write-Host '---------- output -----------'
    $out = Invoke-Interp -Source $Src
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "  >$_" }
}

W 'im_a.txt' @'
data Color = Red | Blue;
def mkOrd f = Ord { cmp = f };
implicit def ordColor = mkOrd (lambda a b -> EQ) :: Ord Color;
def pick a b = min a b;
'@

W 'im_bad.txt' @'
data Shade = Dark | Light;
implicit def ordShade = 3 :: Ord Shade;
'@

W 'im_int.txt' @'
implicit def myInt = Ord { cmp = lambda a b -> GT } :: Ord Int;
'@

Show @'
import "build-tmp/im_a.txt" as M;
M.pick Red Blue;
'@

Show @'
import "build-tmp/im_a.txt" as M;
import "build-tmp/im_a.txt" as M;
M.pick Red Blue;
'@

Show @'
import "build-tmp/im_bad.txt" as B;
data Shade2 = Dark2 | Light2;
'@

Show @'
import "build-tmp/im_int.txt" as I;
min 5 7;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit loc = (mkOrd (lambda a b -> GT) :: Ord Int) in (let g = lambda p q -> min p q in g 5 7);
f 1;
'@

Show @'
data Color = Red | Blue;
import "build-tmp/im_a.txt" as M;
def useC {o :: Ord Color} x = o.cmp x x;
useC Red;
'@
