# scratch explorer #6
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

W 'im_strbad.txt' @'
implicit def myStr = Ord { cmp = lambda a b -> GT } :: Ord String;
def broken = 1 :: String;
'@

Show @'
import "build-tmp/im_strbad.txt" as B;
min "a" "b";
'@

Show @'
import "build-tmp/im_a.txt" as M;
import "build-tmp/im_a.txt" as M;
M.pick M.Red M.Blue;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f {o :: Ord a} x = let d = o in x;
f 3;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f {o :: Ord a} x = (x, o);
'@

Show @'
def mkOrd f = Ord { cmp = f };
data Color = Red | Blue;
implicit def cStr = mkOrd (lambda a b -> EQ) :: Ord String;
def f x = min x x;
f Red;
'@

Show @'
def mkOrd f = Ord { cmp = f };
implicit def outerG = mkOrd (lambda a b -> GT) :: Ord Int;
min 1 2;
'@

Show @'
def mkOrd f = Ord { cmp = f };
let implicit inner = mkOrd (lambda a b -> GT) :: Ord Int in min 1 2;
'@
