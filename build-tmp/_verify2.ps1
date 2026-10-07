# build-tmp\_verify2.ps1 -- root-cause isolations
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

# BUG-1 isolation: with NO enclosing parameter the same let-bound shape works.
V 'iso-let-noparam.txt' @'
def f = let y = min in y;
f 3 4;
'@

# BUG-1 isolation: the wanted var comes from an inner lambda, not from a
# parameter of the enclosing def -> works.
V 'iso-let-innerlam.txt' @'
def f x = let y = (lambda z -> min z z) in (y 1, x);
f 9;
'@

# BUG-1 isolation: the same let at top level (outside any def) -> works.
V 'iso-let-toplevel.txt' @'
let y = min in y 3 4;
'@

# BUG-3 contrast: an implicit parameter that is not mentioned at all.
V 'iso-unused-param.txt' @'
def f {o :: Ord a} x = x;
f 3;
'@

# BUG-3 contrast: the parameter is used but the requirement IS tied to the type.
V 'iso-used-param.txt' @'
def f {o :: Ord a} x y = o.cmp x y;
f 1 2;
'@
