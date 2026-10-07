# scratch explorer #2 -- single-quoted here-strings keep quotes/backslashes literal
$env:PSExecutionPolicyPreference = 'Bypass'
. (Join-Path $PSScriptRoot 'run.ps1') -Exe (Join-Path $PSScriptRoot 'v1.exe')

function Show {
    param([string]$Src)
    Write-Host '---------- program ----------'
    ([System.Text.Encoding]::UTF8.GetString([byte[]][char[]]$Src)) | Out-Null
    $Src -split "`n" | ForEach-Object { Write-Host "  |$_" }
    Write-Host '---------- output -----------'
    $out = Invoke-Interp -Source $Src
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "  >$_" }
}

Show @'
min "a" "b";
max "a" "b";
'@

Show @'
def mkOrd f = Ord { cmp = f };
implicit def oa = mkOrd (lambda a b -> EQ) :: Ord Int;
implicit def ob = mkOrd (lambda a b -> GT) :: Ord Int;
def useM x = min x 0;
'@

Show @'
def mkOrd f = Ord { cmp = f };
implicit def oa = mkOrd (lambda a b -> EQ) :: Ord Int;
implicit def ob = mkOrd (lambda a b -> GT) :: Ord Int;
def useM x = min x x;
useM 5;
'@

Show @'
data Color = Red | Blue;
def useC {o :: Ord Color} x = o.cmp x Red;
def caller1 c = useC c;
caller1 Red;
'@

Show @'
data Box a = Box a;
def useB {o :: Ord a} x = o.cmp x x;
useB (Box 1);
'@

Show @'
def mkOrd f = Ord { cmp = f };
def dup {o :: Ord Int} {o :: Ord String} x = o.cmp x x;
'@

Show @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit loc = mkOrd (lambda a b -> GT) :: Ord Int in min x 0;
f 7;
'@
