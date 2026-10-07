# build-tmp/run.ps1 -- PowerShell probe framework (ASCII only!)
#
# Dot-source it, then use Invoke-Interp / chk / chk_file / Show-Summary.
#   $env:PSExecutionPolicyPreference='Bypass'
#   . build-tmp\run.ps1 -Exe build-tmp\v1.exe
#
# Why not the .sh scripts: Git Bash and WSL are both blocked on this machine
# (bash: couldn't create signal pipe, Win32 error 5 / Wsl E_ACCESSDENIED).
#
# NOTE: this file must stay ASCII-only. PowerShell reads .ps1 as ANSI/GBK here,
#       so any non-ASCII byte inside a string literal breaks parsing.
#
# ---- how the interpreter is invoked -----------------------------------------
# Everything goes through a *file argument* (`exe prog.txt`), not stdin.
# Verified by hand: file mode echoes every statement, same as the REPL.
# Rejected alternatives, all of which silently produced a bare EOF (prompt,
# blank line, "Bye." and nothing else):
#   * .NET StandardInput.Write + Flush + Close
#   * .NET Process running `cmd /c exe < file`
#   * PowerShell pipeline `Get-Content -Raw | & $exe` -- works in the top-level
#     session, but not inside Start-Job (also needed for the timeout wrapper).
# The temp program file must live in the workspace root: `build-tmp\` and
# `test\rev\` refuse newly created *sub*directories on this machine.

[CmdletBinding()]
param(
    [string]$Exe = 'build-tmp/v1.exe',
    [int]$TimeoutSec = 24
)

$ErrorActionPreference = 'Stop'
$script:Exe = (Resolve-Path $Exe).Path
$script:TimeoutSec = $TimeoutSec
$script:Pass = 0
$script:Fail = 0
$script:Failures = New-Object System.Collections.Generic.List[string]
$script:TmpDir = Join-Path (Get-Location).Path 'build-tmp'
$script:Counter = 0

# Run one program (passed as source text) and return its merged stdout+stderr.
function Invoke-Interp {
    param(
        [Parameter(Mandatory)][AllowEmptyString()][string]$Source,
        [string[]]$Args = @(),
        [switch]$Repl
    )
    $script:Counter++
    $file = Join-Path $script:TmpDir ("_probe{0}.txt" -f $script:Counter)
    # `\n` in the table means a real newline; `<BS>` means one literal backslash.
    # Nothing else is decoded: strings like "a\tb" are *language* input and must
    # reach the interpreter verbatim (the .sh probes used `printf %b`, which
    # decoded them -- that is why a naive port produced false failures).
    # Use literal .Replace, not -replace: in a -replace replacement string a
    # backslash is a backreference introducer and does NOT mean one backslash.
    $text = $Source.Replace('<BS>', [string][char]92).Replace('<NL>', "`n").Replace('\n', "`n")
    [System.IO.File]::WriteAllText($file, $text, (New-Object System.Text.UTF8Encoding($false)))
    try {
        if ($Repl) {
            # REPL-only commands (`:t`, `:load`, `:q`) need real stdin. The one
            # method that works here is `cmd /c "type file | exe"`: cmd's own
            # pipe, not a .NET or PowerShell pipe (both unreliable here).
            $out = cmd /c "type `"$file`" | `"$script:Exe`"" 2>&1 | Out-String
        } elseif ($Args.Count -gt 0) {
            $out = & $script:Exe @Args 2>&1 | Out-String
        } else {
            $out = & $script:Exe $file 2>&1 | Out-String
        }
        return ($out -replace "`r", "")
    } finally {
        Remove-Item $file -Force -ErrorAction SilentlyContinue
    }
}

# chk <name> <accept|reject> <needle> <input> [-NotNeedle s]
function chk {
    param(
        [Parameter(Mandatory)][string]$Name,
        [Parameter(Mandatory)][ValidateSet('accept', 'reject')][string]$Want,
        [Parameter(Mandatory)][AllowEmptyString()][string]$Needle,
        [Parameter(Mandatory)][AllowEmptyString()][string]$Source,
        [string]$NotNeedle = '',
        [switch]$ShowOk,
        [switch]$Repl
    )
    $out = Invoke-Interp -Source $Source -Repl:$Repl
    # `<BS>` in a needle means one literal backslash (see Invoke-Interp).
    $needleTxt = $Needle.Replace('<BS>', [string][char]92)
    $ok = $true
    $hasErr = $out -match 'Type Error:|Parse Error:|Eval Error:'
    if ($Want -eq 'reject' -and -not $hasErr) { $ok = $false }
    if ($Want -eq 'accept' -and $hasErr)      { $ok = $false }
    if ($needleTxt -ne '' -and -not $out.Contains($needleTxt)) { $ok = $false }
    if ($NotNeedle -ne '' -and $out.Contains($NotNeedle)) { $ok = $false }

    if ($ok) {
        if ($ShowOk) { Write-Host "  OK $Name" }
        $script:Pass++
    } else {
        Write-Host "  FAIL $Name -- want[$Want] needle[$needleTxt]" -ForegroundColor Red
        ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "      | $_" }
        $script:Fail++
        $script:Failures.Add($Name)
    }
    return $ok
}

# chk_file <name> <corpus> <golden>  -- run on a corpus file, diff stdout
function chk_file {
    param(
        [Parameter(Mandatory)][string]$Name,
        [Parameter(Mandatory)][string]$Corpus,
        [Parameter(Mandatory)][string]$Golden
    )
    if (-not (Test-Path $Corpus)) { Write-Host "  SKIP $Name (no $Corpus)"; return $true }
    if (-not (Test-Path $Golden)) { Write-Host "  SKIP $Name (no $Golden)"; return $true }
    $actual = Invoke-Interp -Source '' -Args @((Resolve-Path $Corpus).Path)
    $expect = ([System.IO.File]::ReadAllText((Resolve-Path $Golden).Path)) -replace "`r", ""

    if ($actual -eq $expect) {
        Write-Host "  OK $Name"
        $script:Pass++
        return $true
    }
    Write-Host "  FAIL $Name -- differs from $Golden" -ForegroundColor Red
    $al = $actual -split "`n"; $el = $expect -split "`n"
    $shown = 0
    for ($i = 0; $i -lt [Math]::Max($al.Count, $el.Count); $i++) {
        $a = if ($i -lt $al.Count) { $al[$i] } else { '<missing>' }
        $e = if ($i -lt $el.Count) { $el[$i] } else { '<missing>' }
        if ($a -ne $e) {
            Write-Host "      line $($i+1)"
            Write-Host "        expect: $e"
            Write-Host "        actual: $a"
            $shown++
            if ($shown -ge 25) { Write-Host '        ...'; break }
        }
    }
    $script:Fail++
    $script:Failures.Add($Name)
    return $false
}

function Show-Summary {
    Write-Host ''
    if ($script:Fail -eq 0) {
        Write-Host "ALL PASS ($($script:Pass) checks)" -ForegroundColor Green
        return $true
    }
    Write-Host "$($script:Fail) FAILED, $($script:Pass) passed:" -ForegroundColor Red
    $script:Failures | ForEach-Object { Write-Host "  - $_" }
    return $false
}

Write-Host "exe: $script:Exe"
