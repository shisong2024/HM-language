# scratch explorer #7 -- eval deps, rec-let, candidate context errors, error kinds
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

W 'im_intB.txt' @'
implicit def otherInt = Ord { cmp = lambda a b -> GT } :: Ord Int;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def useI {o :: Ord Int} x = o.cmp x x;
def callerI v = useI v;
implicit def ordLate = mkOrd (lambda a b -> GT) :: Ord Int;
callerI 3;
'@

Show @'
def mkOrd f = Ord { cmp = f };
implicit def ordFirst = mkOrd (lambda a b -> GT) :: Ord Int;
def useI {o :: Ord Int} x = o.cmp x x;
useI 3;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f = let o = lambda y -> min y y in o 1;
f;
'@

Show @'
let implicit o = min in min 1 2;
'@

Show @'
implicit def ordl {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
'@

Show @'
min;
'@

Show @'
max;
'@

Show @'
import "build-tmp/im_int.txt" as I;
import "build-tmp/im_intB.txt" as J;
min 5 7;
'@
