# build-tmp/xrun.ps1 -- quick exploratory runner (ASCII only).
#   . build-tmp/xrun.ps1
#   XCase -Name 'case1' -Drv '...program...'                       # file mode
#   XCase -Name 'case2' -Mods @{ 'a.txt' = '...'; } -Drv 'import a.txt as A;\n...'
# Every file is written under build-tmp/xc_<name>_*, and stale copies of the
# same case are removed first, so a previous run cannot produce a phantom.
# Imports in the *driver* resolve against the driver's directory, so a driver
# placed at build-tmp/ must import its siblings by bare name.  Paths passed to
# the interpreter therefore use forward slashes (dirOf only knows about '/').

$script:XEnc = New-Object System.Text.UTF8Encoding($false)
$script:XRoot = (Get-Location).Path

function XWrite {
    param([string]$Rel, [string]$Text)
    $t = $Text.Replace('<BS>', '\').Replace('\n', "`n")
    $full = Join-Path $script:XRoot $Rel
    [System.IO.File]::WriteAllText($full, $t, $script:XEnc)
}

function XCase {
    param(
        [Parameter(Mandatory)][string]$Name,
        [Parameter(Mandatory)][AllowEmptyString()][string]$Drv,
        [hashtable]$Mods = @{},
        [string]$Model = 'x',
        [switch]$Quiet
    )
    # Module files are named exactly as the driver imports them (`xc_*.txt`
    # in build-tmp/), so every module for every case lives in one directory and
    # all of them are wiped before each case -- no phantom from a stale file.
    Get-ChildItem -Path (Join-Path $script:XRoot 'build-tmp') -Filter "xc_*.txt" -ErrorAction SilentlyContinue |
        Remove-Item -Force -ErrorAction SilentlyContinue
    foreach ($k in $Mods.Keys) { XWrite ("build-tmp/" + $k) $Mods[$k] }
    XWrite 'build-tmp/xc_drv.txt' $Drv
    $exe = Join-Path $script:XRoot 'build-tmp/v1.exe'
    $out = & $exe 'build-tmp/xc_drv.txt' 2>&1 | Out-String
    $out = $out -replace "`r", ''
    Write-Host "===== $Name ($Model) =====" -ForegroundColor Cyan
    if (-not $Quiet) {
        Write-Host "--- program (build-tmp/xc_drv.txt) ---"
        ($Drv.Replace('<BS>', '\').Replace('\n', "`n") -split "`n") | ForEach-Object { Write-Host "  | $_" }
        foreach ($k in $Mods.Keys) {
            Write-Host "--- module build-tmp/$k ---"
            ($Mods[$k].Replace('<BS>', '\').Replace('\n', "`n") -split "`n") | ForEach-Object { Write-Host "  | $_" }
        }
    }
    Write-Host "--- output ---"
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "  $_" }
    return $out
}
