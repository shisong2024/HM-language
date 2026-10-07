# build-tmp\_verify.ps1 -- direct, by-hand verification of each BUG case.
# Writes one program per case to a named file and runs the exe on it directly
# (`& v1.exe <file>`), printing the raw output.  No harness in the loop.
$env:PSExecutionPolicyPreference = 'Bypass'
$exe = Join-Path $PSScriptRoot 'v1.exe'
function V([string]$file, [string]$text) {
    $p = Join-Path $PSScriptRoot $file
    [System.IO.File]::WriteAllText($p, $text, (New-Object System.Text.UTF8Encoding($false)))
    Write-Host ("----- " + $file + " -----")
    $text.TrimEnd("`n") -split "`n" | ForEach-Object { Write-Host ("  | " + $_) }
    Write-Host "  --- output ---"
    $out = & $exe $p 2>&1 | Out-String
    ($out -replace "`r", "") -split "`n" | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host ("  > " + $_) }
    Write-Host ''
}

V 'bug1.txt' @'
def f x = let y = min x in y;
f 3 4;
'@

V 'bug1b.txt' @'
def outer x = let g = lambda y -> min x y in g x;
outer 3;
'@

V 'bug2.txt' @'
def f x = g x;
def g x = if true then min x x else f x;
f 3;
'@

V 'bug3.txt' @'
def f {o :: Ord a} x = let d = o in x;
f 3;
'@

V 'bug4.txt' @'
min;
'@

V 'bug5.txt' @'
implicit def ordl {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
'@

V 'bug6.txt' @'
data Color = Red | Blue;
def useC {o :: Ord Color} x = o.cmp x x;
'@

V 'bug7.txt' @'
def dup {o :: Ord Int} {o :: Ord String} x = o.cmp x x;
'@

# control cases: the same shapes that must keep working
V 'ok-let-concrete.txt' @'
def f x = let y = min x 0 in y;
f 3;
'@

V 'ok-scc-both-params.txt' @'
def mkOrd f = Ord { cmp = f };
implicit def ordI = mkOrd (lambda a b -> GT) :: Ord Int;
def ev {o :: Ord Int} n = if n == 0 then true else od (n - 1);
def od {o :: Ord Int} n = if n == 0 then false else ev (n - 1);
ev 4;
'@
