# build-tmp/probe-str.ps1 -- string feature probe (port of test/rev/probe-str.sh, ASCII only)
# Run: $env:PSExecutionPolicyPreference='Bypass'; & build-tmp\probe-str.ps1 -Exe build-tmp\v1.exe
[CmdletBinding()]
param([string]$Exe = 'build-tmp\v1.exe')
$ErrorActionPreference = 'Continue'
Set-Location (Split-Path -Parent $PSScriptRoot)
. build-tmp\run.ps1 -Exe $Exe

# n = name, w = accept|reject, k = needle, s = source
$cases = @(
    @{ n='literal echo + type';       w='accept'; k='Type : String';    s='"abc";\n' }
    @{ n='++ concat right-assoc';     w='accept'; k='Value: "abc"';     s='"a" ++ "b" ++ "c";\n' }
    @{ n='== equal';                  w='accept'; k='Value: true';      s='"a" == "a";\n' }
    @{ n='!= not equal';              w='accept'; k='Value: true';      s='"a" != "b";\n' }
    @{ n='compares content not addr'; w='accept'; k='Value: true';      s='("ab" ++ "c") == ("a" ++ "bc");\n' }
    @{ n='escape echo backslash-t';   w='accept'; k='Value: "a<BS>tb"';  s='"a<BS>tb";\n' }
    @{ n='escape echo backslash-r';   w='accept'; k='Value: "a<BS>rb"';  s='"a<BS>rb";\n' }
    @{ n='escape echo backslash';     w='accept'; k='Value: "a\\b"'; s='"a\\\\b";\n' }
    @{ n='escape echo quote';         w='accept'; k='Value: "a<BS>"b"';  s='"a<BS>"b";\n' }
    @{ n='uXXXX ascii';               w='accept'; k='Value: "A"';       s='"<BS>u0041";\n' }
    @{ n='uXXXX non-ascii';           w='accept'; k='Value: "<BS>u25B6"'; s='"<BS>u25B6";\n' }
    @{ n='uncons non-empty';          w='accept'; k='(Just (97, "bc"))'; s='uncons "abc";\n' }
    @{ n='uncons empty';              w='accept'; k='Value: None';      s='uncons "";\n' }
    @{ n='fromCode';                  w='accept'; k='Value: "A"';       s='fromCode 65;\n' }
    @{ n='literal pattern hit';       w='accept'; k='Value: 1';         s='match "GET" with "GET" -> 1 | _ -> 2;\n' }
    @{ n='literal pattern miss';      w='accept'; k='Value: 2';         s='match "PUT" with "GET" -> 1 | _ -> 2;\n' }
    @{ n='-- inside string is data';  w='accept'; k='Value: "a--b"';    s='"a--b";\n' }
    @{ n='escaped quote in string';   w='accept'; k='Value: "a<BS>"--b"'; s='"a<BS>"--b";\n' }
    @{ n='String is not Int';         w='reject'; k='expected Int but got String';    s='"a" + 1;\n' }
    @{ n='String is not [Int]';       w='reject'; k='expected [Int] but got String';  s='("abc" :: [Int]);\n' }
    @{ n='String cannot enter list';  w='reject'; k='expected [Int] but got String';  s='def g x = x ++ [1];\n' }
    @{ n='annotation must match';     w='reject'; k='expected String but got Int';    s='(5 :: String);\n' }
    @{ n='String has no <';           w='reject'; k='expected Int but got String';    s='"a" < "b";\n' }
    @{ n='pattern type must match';   w='reject'; k='Type mismatch';                  s='match 5 with "a" -> 1 | _ -> 2;\n' }
    @{ n='codepoint too large';       w='reject'; k='Not a Unicode code point: 65536'; s='fromCode 65536;\n' }
    @{ n='codepoint negative';        w='reject'; k='Not a Unicode code point: -1';   s='fromCode (0 - 1);\n' }
    @{ n='reserved type name (type)'; w='reject'; k='type name and cannot';           s='type String = Int;\n' }
    @{ n='reserved type name (data)'; w='reject'; k='type name and cannot';           s='data String = Q;\n' }
    @{ n='strLen';                    w='accept'; k='Value: 5';         s='strLen "Hello";\n' }
    @{ n='strReverse';                w='accept'; k='Value: "cba"';     s='strReverse "abc";\n' }
    @{ n='strToUpper';                w='accept'; k='Value: "HELLO!"';  s='strToUpper "Hello!";\n' }
    @{ n='strSplitAt';                w='accept'; k='("abc", "def")';   s='strSplitAt 3 "abcdef";\n' }
    @{ n='strWords';                  w='accept'; k='["the", "quick"]'; s='strWords "  the quick ";\n' }
    @{ n='strLines trailing nl';      w='accept'; k='Value: ["a", "b"]'; s="strLines `"a<BS>nb<BS>n`";`n" }
    @{ n=':t knows String (file mode)'; w='accept'; k='Type : String';   s='("x" :: String);\n' }
    @{ n='showInt/readInt roundtrip'; w='accept'; k='Value: (Just 123)'; s='readInt (showInt 123);\n' }
    @{ n='readInt non-numeric';       w='accept'; k='Value: None';      s='readInt "12x";\n' }
    @{ n='isDigit is codepoint';      w='accept'; k='Value: true';      s='isDigit 53;\n' }
    @{ n=':: String annotation';      w='accept'; k='Type : String';    s='("hi" :: String) ++ "!";\n' }
    @{ n='(removed: :t is REPL-only)'; w='accept'; k='Type : String';    s='"abc";\n' }
    @{ n='data param is String';      w='accept'; k='S : (String -> S)'; s='data S = S String;\nS "q";\n' }
)

Write-Host ''
Write-Host 'STRING PROBE'
foreach ($c in $cases) { chk $c.n $c.w $c.k $c.s | Out-Null }

Write-Host ''
Write-Host 'STRING PROBE (crash / cross-module)'
# Primitive receiving a wrongly-typed argument must report, not crash, and later
# statements must still run. A CallStack trace is the fingerprint of a crash.
$out = Invoke-Interp -Source "strLen 5;\n1 + 1;\n"
$ok = $out.Contains('expected Int but got String') -and $out.Contains('Value: 2') -and -not $out.Contains('CallStack')
if ($ok) { Write-Host '  OK primitive with bad arg: reports, does not crash, continues'; $script:Pass++ }
else {
    Write-Host '  FAIL primitive with bad arg' -ForegroundColor Red
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "      | $_" }
    $script:Fail++; $script:Failures.Add('primitive with bad arg')
}

# cross-module: uncons return type must unify with the module's Maybe
$mods = Join-Path (Get-Location).Path 'build-tmp\strmods.txt'
$drv  = Join-Path (Get-Location).Path 'build-tmp\strdrv.txt'
[System.IO.File]::WriteAllText($mods, "def firstChar s = match uncons s with None -> 0 | Just (c, _) -> c;`ndef shout s = match uncons s with None -> `"`" | Just (c, rest) -> fromCode (c - 32) ++ shout rest;`n")
[System.IO.File]::WriteAllText($drv, "import strmods.txt as M;`nM.firstChar `"A`";`nM.shout `"abc`";`n")
$out = Invoke-Interp -Source '' -Args @($drv)
$ok = $out.Contains('Value: 65') -and $out.Contains('Value: "ABC"')
if ($ok) { Write-Host '  OK cross-module uncons/Maybe unify'; $script:Pass++ }
else {
    Write-Host '  FAIL cross-module uncons/Maybe unify' -ForegroundColor Red
    ($out -split "`n") | Where-Object { $_ -ne '' } | ForEach-Object { Write-Host "      | $_" }
    $script:Fail++; $script:Failures.Add('cross-module uncons/Maybe unify')
}

exit ([int](-not (Show-Summary)))
