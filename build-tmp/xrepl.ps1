# build-tmp/xrepl.ps1 -- interactive REPL driver for probes (ASCII only!)
# Dot-source, then:  $p = Start-X; Send-X $p '1+1;'; Read-X $p; Stop-X $p
# This exists because a plain PowerShell pipeline buffers stdin and delivers
# everything at once, so an edit made mid-session is seen by the *first* load.

$script:XPend = $null
$script:XEnc = New-Object System.Text.UTF8Encoding($false)

function Set-XFile {
    param([string]$Path, [string]$Text)
    # `\n` means a real newline; <BS> means one literal backslash.
    $t = $Text.Replace('<BS>', '\').Replace('\n', "`n")
    [System.IO.File]::WriteAllText((Join-Path $PWD $Path), $t, $script:XEnc)
}

function Start-X {
    param([string]$Exe = 'build-tmp/v1.exe')
    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = (Join-Path $PWD $Exe)
    $psi.RedirectStandardInput = $true
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.UseShellExecute = $false
    $psi.StandardOutputEncoding = [System.Text.Encoding]::UTF8
    $psi.StandardErrorEncoding = [System.Text.Encoding]::UTF8
    $p = [System.Diagnostics.Process]::Start($psi)
    $script:XPend = $null
    Start-Sleep -Milliseconds 250
    return $p
}

function Send-X {
    param($P, [string]$Line)
    $P.StandardInput.WriteLine($Line)
    $P.StandardInput.Flush()
}

# Collect output until the process has been quiet for $Quiet ms.
function Read-X {
    param($P, [int]$Quiet = 400)
    $lines = New-Object System.Collections.Generic.List[string]
    while ($true) {
        if ($null -eq $script:XPend) { $script:XPend = $P.StandardOutput.ReadLineAsync() }
        if ($script:XPend.Wait($Quiet)) {
            $r = $script:XPend.Result
            $script:XPend = $null
            if ($null -eq $r) { break } else { $lines.Add($r) }
        } else { break }
    }
    return $lines
}

# Send one line and return its output (list of strings).
function Ask-X {
    param($P, [string]$Line, [int]$Quiet = 400)
    Send-X $P $Line
    return (Read-X $P $Quiet)
}

function Stop-X {
    param($P)
    try { Send-X $P ':q' } catch { }
    Start-Sleep -Milliseconds 250
    try { $P.StandardInput.Close() } catch { }
    try { $null = $P.WaitForExit(3000) } catch { }
    if (-not $P.HasExited) { try { $P.Kill() } catch { } }
    try { $err = $P.StandardError.ReadToEnd(); if ($err) { Write-Host "STDERR: $err" } } catch { }
    $script:XPend = $null
}
