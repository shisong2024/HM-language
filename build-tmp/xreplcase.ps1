# build-tmp/xreplcase.ps1 -- REPL-session cases for reload / unload probes.
# Dot-source after xrepl.ps1.  Steps are a list of hashtables:
#   @{ send = 'line' }             -> send, print the reply
#   @{ edit = 'build-tmp/x.txt'; text = '...' }   -> overwrite a module file
#   @{ label = 'name' }            -> section header
# ASCII only.

function XShow {
    param($Lines, [string]$Label)
    $t = (($Lines | Where-Object { $_ -ne '' -and $_ -ne '> ' }) -join ' ~ ')
    $t = $t -replace '> > ', '> '
    Write-Host ("  {0,-34} {1}" -f $Label, $t)
}

function XSteps {
    param([Parameter(Mandatory)][array]$Steps, [string]$Exe = 'build-tmp/v1.exe')
    $p = Start-X -Exe $Exe
    try {
        foreach ($s in $Steps) {
            if ($s.ContainsKey('label')) { Write-Host ("--- " + $s['label']) -ForegroundColor Cyan; continue }
            if ($s.ContainsKey('edit')) { Set-XFile $s['edit'] $s['text']; Write-Host "  [wrote $($s['edit'])]"; continue }
            $r = Ask-X $p $s['send']
            XShow $r $s['send']
        }
    } finally { Stop-X $p }
}
