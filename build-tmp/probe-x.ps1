# build-tmp/probe-x.ps1 -- adversarial probe for the SEAMS between features:
# records / type synonyms / import-as-alias / strings / ADTs / pattern matching
# / custom operators / implicit parameters.
#
# ASCII ONLY (this box reads .ps1 as GBK).
# Run from the repo root:
#     $env:PSExecutionPolicyPreference='Bypass'
#     & build-tmp\probe-x.ps1
#
# Semantics: every check asserts what README.md and the published probes
# (test/rev/probe-rec.sh, probe-syn.sh, probe-r3.sh, probe-str.sh) promise.
# A FAIL is therefore a suspected bug.  Check names carry the id used in the
# write-up (A..I).  Ids marked [info] document behaviour that is surprising
# but defensible (a tradeoff), not a bug.
#
# Harness conventions (same as build-tmp/run.ps1):
#   `\n`   in a source string = one real newline
#   `<BS>` in a source string = one literal backslash
#   A *language level* newline escape cannot be written as `\n` here (the
#   harness rewrites it first) -- use `\u000A` instead, which the language's
#   own escape parser decodes.
#
# Import resolution: a module import is resolved against the *importing file's*
# directory, and dirOf only understands "/", so this probe always names the
# driver with forward slashes.  Check F pins exactly that problem.

[CmdletBinding()]
param(
    [string]$Exe = 'build-tmp/v1.exe',
    [switch]$ShowOutput
)

$ErrorActionPreference = 'Stop'

if (-not (Test-Path 'utils/prelude.txt')) {
    Write-Host 'Run this from the repository root (utils/prelude.txt not found).' -ForegroundColor Red
    exit 2
}

$script:Enc  = New-Object System.Text.UTF8Encoding($false)
$script:Root = (Get-Location).Path
$script:Exe  = (Resolve-Path $Exe).Path
$script:Pass = 0
$script:Fail = 0
$script:Fails = New-Object System.Collections.Generic.List[string]
$script:Ctr  = 0

# ---------------------------------------------------------------- file plumbing

function PxClean {
    # module/driver files are px<counter>_<key>.txt; wiping every px*.txt before
    # each case is what keeps a stale module from producing a phantom failure.
    Get-ChildItem -Path (Join-Path $script:Root 'build-tmp') -Filter 'px*.txt' -ErrorAction SilentlyContinue |
        Remove-Item -Force -ErrorAction SilentlyContinue
}

function PxWrite {
    param([string]$Rel, [string]$Text)
    $t = $Text.Replace('<BS>', '\').Replace('\n', "`n")
    [System.IO.File]::WriteAllText((Join-Path $script:Root $Rel), $t, $script:Enc)
}

# Run one program (optionally with sibling modules) and return merged output.
function PxRun {
    param(
        [Parameter(Mandatory)][AllowEmptyString()][string]$Drv,
        [hashtable]$Mods = @{},
        [string]$DrvPath = ''      # override to exercise backslash paths (check F)
    )
    $script:Ctr++
    $c = $script:Ctr
    PxClean
    foreach ($k in $Mods.Keys) { PxWrite ("build-tmp/px${c}_$k") $Mods[$k].Replace('<N>', "$c") }
    $Drv = $Drv.Replace('<N>', "$c")     # module files are px<counter>_<key>
    if ($DrvPath -eq '') {
        $name = "build-tmp/px${c}_drv.txt"
    } else {
        $name = $DrvPath.Replace('<N>', "$c")
    }
    PxWrite ($name -replace '\\', '/') $Drv
    $out = & $script:Exe $name 2>&1 | Out-String
    return ($out -replace "`r", '')
}

function PxLines { param([string]$Out) return @(($Out -split "`n") | Where-Object { $_ -ne '' }) }

function PxLastValue {
    param([string]$Out)
    $v = (PxLines $Out) | Where-Object { $_ -like 'Value: *' }
    if ($v.Count -eq 0) { return '<none>' }
    return $v[-1]
}

function PxFail {
    param([string]$Name, [string]$Why, [string]$Out)
    $script:Fail++
    $script:Fails.Add($Name)
    Write-Host "FAIL  $Name -- $Why" -ForegroundColor Red
    (PxLines $Out) | Select-Object -First 14 | ForEach-Object { Write-Host "        | $_" }
}

function PxOk { param([string]$Name) $script:Pass++; Write-Host "  ok  $Name" }

# chk <name> <accept|reject> <needle> <source> [-NotNeedle s] [-Mods @{}]
function chk {
    param(
        [Parameter(Mandatory)][string]$Name,
        [Parameter(Mandatory)][ValidateSet('accept', 'reject')][string]$Want,
        [Parameter(Mandatory)][AllowEmptyString()][string]$Needle,
        [Parameter(Mandatory)][AllowEmptyString()][string]$Source,
        [string]$NotNeedle = '',
        [hashtable]$Mods = @{},
        [string]$DrvPath = ''
    )
    $out = PxRun -Drv $Source -Mods $Mods -DrvPath $DrvPath
    $Needle = $Needle.Replace('<BS>', '\')      # needles name literal backslashes
    $hasErr = $out -match 'Type Error:|Parse Error:|Eval Error:'
    $why = @()
    if ($Want -eq 'reject' -and -not $hasErr) { $why += 'expected a diagnosed error, none appeared' }
    if ($Want -eq 'accept' -and $hasErr)      { $why += 'unexpected error' }
    if ($Needle -ne '' -and -not $out.Contains($Needle)) { $why += "missing [$Needle]" }
    if ($NotNeedle -ne '' -and $out.Contains($NotNeedle)) { $why += "must not contain [$NotNeedle]" }
    if ($why.Count -eq 0) { PxOk $Name } else { PxFail $Name ($why -join '; ') $out }
}

# --------------------------------------------------------- interactive plumbing
# A plain PowerShell pipeline delivers stdin all at once, so an edit made
# mid-session is seen by the *first* :load -- reload checks need a real pty-less
# interactive process with an explicitly flushed stdin.

$script:Pend = $null

function Start-Px {
    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = $script:Exe
    $psi.RedirectStandardInput = $true
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.UseShellExecute = $false
    $psi.StandardOutputEncoding = [System.Text.Encoding]::UTF8
    $psi.StandardErrorEncoding = [System.Text.Encoding]::UTF8
    $p = [System.Diagnostics.Process]::Start($psi)
    $script:Pend = $null
    Start-Sleep -Milliseconds 250
    return $p
}

function Ask-Px {
    param($P, [string]$Line, [int]$Quiet = 400)
    $P.StandardInput.WriteLine($Line)
    $P.StandardInput.Flush()
    $lines = New-Object System.Collections.Generic.List[string]
    while ($true) {
        if ($null -eq $script:Pend) { $script:Pend = $P.StandardOutput.ReadLineAsync() }
        if ($script:Pend.Wait($Quiet)) {
            $r = $script:Pend.Result
            $script:Pend = $null
            if ($null -eq $r) { break } else { $lines.Add($r) }
        } else { break }
    }
    return (($lines | Where-Object { $_ -ne '> ' -and $_ -notlike 'HM interpreter*' }) -join "`n")
}

function Stop-Px {
    param($P)
    try { $P.StandardInput.WriteLine(':q'); $P.StandardInput.Flush() } catch { }
    Start-Sleep -Milliseconds 200
    try { $P.StandardInput.Close() } catch { }
    try { $null = $P.WaitForExit(3000) } catch { }
    if (-not $P.HasExited) { try { $P.Kill() } catch { } }
    try { $e = $P.StandardError.ReadToEnd(); if ($e) { Write-Host "stderr: $e" -ForegroundColor DarkGray } } catch { }
    $script:Pend = $null
}

# Run a REPL session.  $Ops is a list of hashtables:
#   @{ send = 'line' }                    -> send, collect the reply
#   @{ edit = 'build-tmp/px_mod.txt'; text = '...' }  -> rewrite a module file
# Returns one string per send (in order).
function ReplOps {
    param([array]$Ops)
    $p = Start-Px
    $outs = New-Object System.Collections.Generic.List[string]
    try {
        foreach ($o in $Ops) {
            if ($o.ContainsKey('edit')) { PxWrite $o['edit'] $o['text']; continue }
            $outs.Add((Ask-Px $p $o['send']))
        }
    } finally { Stop-Px $p }
    return , $outs.ToArray()
}

function Check-Session {
    param(
        [string]$Name,
        [array]$Ops,
        [int[]]$AssertIndex,      # which replies to inspect (1-based)
        [string]$Want = 'accept', # accept = no error, reject = must carry an error
        [string]$Needle = '',
        [string]$NotNeedle = ''
    )
    $outs = ReplOps -Ops $Ops
    $Needle = $Needle.Replace('<BS>', '\')
    $sel = @()
    foreach ($i in $AssertIndex) { if ($i -le $outs.Count) { $sel += $outs[$i - 1] } }
    $out = $sel -join "`n---`n"
    $hasErr = $out -match 'Type Error:|Parse Error:|Eval Error:'
    $why = @()
    if ($Want -eq 'reject' -and -not $hasErr) { $why += 'expected a diagnosed error, none appeared' }
    if ($Want -eq 'accept' -and $hasErr)      { $why += 'unexpected error' }
    if ($Needle -ne '' -and -not $out.Contains($Needle)) { $why += "missing [$Needle]" }
    if ($NotNeedle -ne '' -and $out.Contains($NotNeedle)) { $why += "must not contain [$NotNeedle]" }
    if ($why.Count -eq 0) { PxOk $Name } else { PxFail $Name ($why -join '; ') $out }
}

# ---------------------------------------------------------------------- header
Write-Host "probe-x: seam probe against $($script:Exe)" -ForegroundColor Cyan
Write-Host ''

# ===========================================================================
# A. multi-constructor records: the generated fallback arm ($fieldErr)
#    Documented behaviour: `p.x` on a value whose data type has several
#    constructors is a *runtime* `NoSuchField` (test/rev/probe-rec.sh E).
#    The desugarer emits a fallback arm calling the primitive `$fieldErr`.
# ===========================================================================
Write-Host 'A. multi-constructor records / generated fallback arm' -ForegroundColor Yellow

chk 'A1 access on the right ctor' accept 'Value: 1' `
    'data T = A | B { x :: Int, y :: Bool };\n(B { x = 1, y = false }).x;\n'
chk 'A2 update on the right ctor' accept 'Value: (B 2 false)' `
    'data T = A | B { x :: Int, y :: Bool };\n((B { x = 1, y = false }) { x = 2 });\n'
chk 'A3 README widen example' accept 'Value: 4' `
    'data Shape = Circle { radius :: Int } | Rectangle { width :: Int, height :: Int };\ndef widen s = s { width = s.width + 1 };\nwiden (Rectangle { width = 1, height = 2 }).width;\n'
chk 'A4 getter defined at top level' accept 'def f : (T -> Int)' `
    'data T = A | B { x :: Int, y :: Bool };\ndef f t = t.x;\n1;\n'
chk 'A5 hand written fallback works' accept 'Value: 0' `
    'data T = A | B { x :: Int, y :: Bool };\ndef f t = match t with B { x = v } -> v | other -> 0;\nf (B { x = 1, y = false });\nf A;\n'
chk 'A6 single-ctor access + update (control)' accept 'Value: (W 9)' `
    'data W = W { w1 :: Int };\ndef f t = t.w1;\ndef u t = t { w1 = 9 };\nf (W { w1 = 7 });\nu (W { w1 = 1 });\n'
chk 'A7 wrong ctor is a runtime NoSuchField' reject 'is not a field of A' `
    'data T = A | B { x :: Int, y :: Bool };\ndef f t = t.x;\nf A;\n'
Check-Session -Name 'A8 :t on a 2-ctor getter' -Ops @(
    @{ send = 'data T = A | B { x :: Int, y :: Bool };' }
    @{ send = 'def f t = t.x;' }
    @{ send = ':t f;' }
) -AssertIndex 3 -Want accept -Needle '(T -> Int)'

# ===========================================================================
# B. record field table: keys are the bare field name and are NOT qualified
#    across modules, while the agreement check compares only the *base* ctor
#    name + index + arity.
# ===========================================================================
Write-Host 'B. cross-module / cross-batch field collisions' -ForegroundColor Yellow

$bMods = @{
    'a.txt' = 'data C = C { x :: Int };\ndef mkA n = C { x = n };\ndef getA c = c.x;\n'
    'b.txt' = 'data C = C { x :: Bool };\ndef mkB b = C { x = b };\ndef getB c = c.x;\n'
}
chk 'B1 A then B: A field still reachable' accept 'Value: 2' `
    'import px<N>_a.txt as A;\nimport px<N>_b.txt as B;\n(A.mkA 2).x;\n' -Mods $bMods
chk 'B2 B then A: B field still reachable' accept 'Value: true' `
    'import px<N>_b.txt as B;\nimport px<N>_a.txt as A;\n(B.mkB true).x;\n' -Mods $bMods

$b3mod = @{ 'mod.txt' = 'data C = C { x :: Int, y :: Bool };\ndef mk n = C { x = n, y = false };\n' }
chk 'B3 module-internal field access (control)' accept 'def g : (A.C -> Int)' `
    'import px<N>_mod.txt as A;\ndef g c = c.x;\nA.mk 5;\n' -Mods $b3mod
# The stronger form (local record then a clashing import) needs REPL ordering:
PxClean
PxWrite 'build-tmp/px_b4.txt' 'data C = C { x :: Int, y :: Bool };\ndef mk n = C { x = n, y = false };\n'
Check-Session -Name 'B4 local record survives a clashing import' -Ops @(
    @{ send = 'data C = C { x :: Int };' }
    @{ send = '(C { x = 1 }).x;' }
    @{ send = 'import build-tmp/px_b4.txt as A;' }
    @{ send = '(C { x = 2 }).x;' }
) -AssertIndex 4 -Want accept -Needle 'Value: 2'
Check-Session -Name 'B5 clashing import is diagnosed' -Ops @(
    @{ send = 'data C = C { x :: Int };' }
    @{ send = 'import build-tmp/px_b4.txt as A;' }
) -AssertIndex 2 -Want reject -Needle 'already a field of'
chk 'B6 a FAILED module must not steal a field' accept 'Value: 1' `
    'import px<N>_a.txt as A;\nimport px<N>_bad.txt as X;\n(A.mkA 1).x;\n' `
    -Mods @{ 'a.txt' = 'data C = C { x :: Int };\ndef mkA n = C { x = n };\n'
             'bad.txt' = 'data C = C { x :: Bool };\ndef boom = 1 + true;\n' }

# ===========================================================================
# C. state that a FAILED batch / FAILED import leaves behind
#    (session tables are updated before type checking)
# ===========================================================================
Write-Host 'C. failed batch / failed import leaks' -ForegroundColor Yellow

Check-Session -Name 'C1 failed batch keeps no synonym' -Ops @(
    @{ send = 'type T = Int; def bad = 1 + true;' }
    @{ send = '1 :: T;' }
) -AssertIndex 2 -Want reject
Check-Session -Name 'C2 failed batch keeps no record field' -Ops @(
    @{ send = 'data C = C { x :: Int }; def bad = 1 + true;' }
    @{ send = 'def h n = n.x;' }
) -AssertIndex 2 -Want reject -Needle 'unknown field'
Check-Session -Name 'C3 failed batch keeps no operator fixity' -Ops @(
    @{ send = 'def (<+>) a b = a + b; infixl 6 <+>; def bad = 1 + true;' }
    @{ send = '1 <+> 2;' }
) -AssertIndex 2 -Want reject -Needle 'Parse Error'
chk 'C4 failed import must release its alias' reject 'already used for' `
    'import px<N>_bad.txt as X;\nimport px<N>_ok.txt as X;\nX.z;\n' `
    -Mods @{ 'bad.txt' = 'def boom = 1 + true;\n'; 'ok.txt' = 'def z = 1;\n' }

# ===========================================================================
# D. custom operators: global, never qualified, and their fixity table is
#    session wide.  unload() only strips "<Alias>." keys.
# ===========================================================================
Write-Host 'D. custom operators x import-as-alias' -ForegroundColor Yellow

chk 'D1 implicit def operator is usable (like `def`)' accept 'Value: 3' `
    'implicit def (<+>) a b = a + b;\n1 <+> 2;\n'
chk 'D2 implicit def operator inside a module' accept 'Value: 3' `
    'import px<N>_io.txt as I;\n1 <+> 2;\n' -Mods @{ 'io.txt' = 'implicit def (<+>) a b = a + b;\n' }

PxClean
PxWrite 'build-tmp/px_d_op.txt'     'def (<+>) a b = a + b;\ninfixl 6 <+>;\ndef v = 1;\n'
PxWrite 'build-tmp/px_d_op_drv.txt' 'import px_d_op.txt as O;\n'
Check-Session -Name 'D3 reload drops the operator' -Ops @(
    @{ send = ':load build-tmp/px_d_op_drv.txt' }
    @{ send = '1 <+> 2;' }
    @{ edit = 'build-tmp/px_d_op.txt'; text = 'def v = 1;\n' }
    @{ send = ':load build-tmp/px_d_op_drv.txt' }
    @{ send = '1 <+> 2;' }
) -AssertIndex 4 -Want reject

# Same session shape, second half: the dropped fixity must be gone too.
PxWrite 'build-tmp/px_d_op.txt' 'def (<+>) a b = a + b;\ninfixl 6 <+>;\ndef v = 1;\n'
Check-Session -Name 'D4 reload drops the operator fixity' -Ops @(
    @{ send = ':load build-tmp/px_d_op_drv.txt' }
    @{ send = '1 <+> 2;' }
    @{ edit = 'build-tmp/px_d_op.txt'; text = 'def v = 1;\n' }
    @{ send = ':load build-tmp/px_d_op_drv.txt' }
    @{ send = '1 <+> 2;' }
) -AssertIndex 4 -Want reject -Needle 'Parse Error'

# Import order must not silently re-associate an expression in the importing file.
$dMods = @{
    'oa.txt' = 'def (<+>) a b = a + b;\ninfixl 6 <+>;\ndef fa x = x <+> 10;\n'
    'ob.txt' = 'def (<+>) a b = a * b;\ninfixl 7 <+>;\ndef fb x = x <+> 10;\n'
}
$o1 = PxLastValue (PxRun -Drv 'import px<N>_oa.txt as A;\nimport px<N>_ob.txt as B;\n1 <+> 2 * 3;\n' -Mods $dMods)
$o2 = PxLastValue (PxRun -Drv 'import px<N>_ob.txt as B;\nimport px<N>_oa.txt as A;\n1 <+> 2 * 3;\n' -Mods $dMods)
if ($o1 -eq $o2) {
    PxOk 'D5 import order cannot change the parse'
} else {
    PxFail 'D5 import order cannot change the parse' "A,B gives $o1 but B,A gives $o2 (same expression)" ''
}

# ===========================================================================
# E. type synonyms at the seams
# ===========================================================================
Write-Host 'E. type synonyms at the seams' -ForegroundColor Yellow

chk 'E1 synonym of a synonym in a record field' accept 'Value: 3' `
    'type I = Int;\ntype J = I;\ndata R = R { x :: J, y :: I };\n(R { x = 1, y = 2 }).x + (R { x = 1, y = 2 }).y;\n'
chk 'E2 synonym in a data ctor parameter' accept 'Value: (D (1, 2))' `
    'type P = (Int, Int);\ndata U = D P;\nD (1, 2);\n'
chk 'E3 synonym in an operator body annotation' accept 'Value: 3' `
    'type I = Int;\ndef (<+>) a b = (a + b) :: I;\ninfixl 6 <+>;\n1 <+> 2;\n'
chk 'E4 synonym in a match pattern position' accept 'Value: 3' `
    'type M = Maybe Int;\ndef f x = match (x :: M) with Just n -> n | None -> 0;\nf (Just 3);\n'
chk 'E5 synonym in an implicit candidate annotation' accept 'Value: 6' `
    'data Ord2 a = Ord2 { cmp2 :: a -> a -> Int };\ntype OI = Ord2 Int;\nimplicit def oi = Ord2 { cmp2 = lambda a b -> a + b } :: OI;\ndef f {o :: OI} x = o.cmp2 x 1;\nf 5;\n'
chk 'E6 cross-module synonym + record + operator' accept 'Value: 4' `
    'import px<N>_s2.txt as B;\n1 :: B.I2;\n(B.mk 4).x;\n1 <+> 2;\n' `
    -Mods @{ 's1.txt' = 'type I = Int;\n'
             's2.txt' = 'import px<N>_s1.txt as A;\ntype I2 = A.I;\ndata R = R { x :: I2 };\ndef mk n = R { x = n };\ndef (<+>) a b = (a + b) :: A.I;\ninfixl 6 <+>;\n' }
chk 'E7 == on a record with synonym fields' accept 'Value: false' `
    'type SS = Int;\ndata S = S { s :: SS };\ndef eq a b = (a :: S) == b;\neq (S { s = 1 }) (S { s = 2 });\n'
# A data type and a type synonym may share a name; the synonym then wins in
# every type position, so the data type cannot be named in an annotation.
# Either outcome is fine as long as the clash is *diagnosed* (compare
# DuplicateData / DuplicateDef) instead of silently shadowing.
chk 'E8 synonym vs data type name must be diagnosed' reject 'synonym' `
    'type Pair = Int;\ndata Pair = Pair Int;\n(Pair 1) :: Pair;\n'
chk 'E9 duplicate synonym name in one batch [info]' accept 'Value: true' `
    'type T = Int;\ntype T = Bool;\n(true :: T);\n'
# The same shadowing reaches the prelude's own data types: a user synonym
# named `Maybe` makes every `:: Maybe` annotation mean `Int`.
chk 'E10 synonym may not hijack a prelude type name' reject 'synonym' `
    'type Maybe = Int;\ndef g x = match (x :: Maybe) with Just n -> n | None -> 0;\n'

# ===========================================================================
# F. import paths: dirOf only knows "/"
# ===========================================================================
Write-Host 'F. import paths (dirOf and the Windows separator)' -ForegroundColor Yellow

chk 'F1 backslash driver finds its sibling module' accept 'Value: 42' `
    'import px<N>_sib.txt as M;\nM.v;\n' `
    -Mods @{ 'sib.txt' = 'def v = 42;\n' } -DrvPath 'build-tmp\px<N>_drv.txt'
chk 'F2 backslash driver must not load a CWD file' reject 'Cannot read test.txt' `
    'import test.txt as T;\nT.x;\n' -DrvPath 'build-tmp\px<N>_drv.txt' -NotNeedle 'def T.x'

# ===========================================================================
# G. exhaustiveness: `covered` checks each argument column independently
# ===========================================================================
Write-Host 'G. exhaustiveness vs the cartesian product' -ForegroundColor Yellow

chk 'G1 positional 2-arg ctor (same bug, no record)' reject 'not exhaustive' `
    'data L = A | B;\ndata T = C L Bool;\ndef f t = match t with C A true -> 1 | C B false -> 2;\n1 + 1;\n' -NotNeedle 'Value: 2'
chk 'G2 record 2-field ctor' reject 'not exhaustive' `
    'data L = A | B;\ndata T = C { p :: L, q :: Bool };\ndef f t = match t with C { p = A, q = true } -> 1 | C { p = B, q = false } -> 2;\n1 + 1;\n' -NotNeedle 'Value: 2'

# ===========================================================================
# H. controls: seams that DO hold (these must stay green)
# ===========================================================================
Write-Host 'H. controls that must hold' -ForegroundColor Yellow

chk 'H1 string with {}::;-- in a record field' accept 'Value: "a{b}c::d;e"' `
    'data S = S { s :: String };\n(S { s = "a{b}c::d;e" }).s;\n'
chk 'H2 -- and escaped quote inside a string' accept 'Value: "a<BS>"--b"' `
    'data S = S { s :: String };\ndef mk = S { s = "x--y" };\nmk.s;\ndef mk2 = S { s = "a<BS>"--b" };\nmk2.s;\n'
chk 'H3 string pattern with , } = inside a record pattern' accept 'Value: 1' `
    'data S = S { s :: String };\ndef f t = match t with S { s = "a,b}c=d" } -> 1 | S { s = _ } -> 2;\nf (S { s = "a,b}c=d" });\n'
chk 'H4 newline escape in a record field' accept 'Value: 3' `
    'data S = S { s :: String };\ndef mk = S { s = "a<BS>u000Ab" };\nstrLen mk.s;\n'
chk 'H5 full-line -- comment inside a record decl' accept 'Value: "q"' `
    'data S = S {\n  -- the string field\n  s :: String,\n  -- the count\n  n :: Int\n};\ndef mk = S { s = "q", n = 1 };\nmk.s;\n'
chk 'H6 trailing -- after code is rejected' reject 'must be at the start of a line' `
    'def x = 1; -- nope\nx;\n'
chk 'H7 refutable let is rejected' reject 'may fail to match' `
    'data S = S { s :: String, n :: Int };\nlet S { n = k } = S { s = "q", n = 1 } in k;\n'
chk 'H8 tuple destructuring let works' accept 'Value: 3' `
    'let (a, b) = (1, 2) in a + b;\n'
chk 'H9 cross-module record + string + list field' accept 'Value: 104' `
    'import px<N>_m.txt as M;\nM.get (M.S { s = "hi", xs = [1] });\nM.first (M.S { s = "hi", xs = [1] });\n' `
    -Mods @{ 'm.txt' = 'data S = S { s :: String, xs :: [Int] };\ndef get s = s.s;\ndef first s = match uncons (get s) with Just (c, _) -> c | None -> 0;\n' }
chk 'H10 self import is idempotent' accept 'Value: 1' `
    'import px<N>_i.txt as Z;\nimport px<N>_i.txt as Z;\nZ.zz;\n' -Mods @{ 'i.txt' = 'def zz = 1;\n' }

# ===========================================================================
# I. tradeoffs (surprising, but consistent with the documented design)
# ===========================================================================
Write-Host 'I. documented-but-surprising tradeoffs [info]' -ForegroundColor Yellow

chk 'I1 == on a record holding a closure' reject 'are not comparable' `
    'data O = O { fn :: Int -> Int };\n(O { fn = lambda x -> x }) == (O { fn = lambda x -> x });\n'
chk 'I2 a user field may not reuse a prelude field name' reject 'already a field of' `
    'data Foo = Foo { cmp :: Int };\n1;\n'
chk 'I3 a module candidate cannot shadow the prelude candidate' reject 'More than one implicit value matches' `
    'import px<N>_ord.txt as O;\nmin 1 2;\n' -Mods @{ 'ord.txt' = 'implicit def oi = Ord { cmp = lambda a b -> if a < b then LT else GT };\n' }

# ------------------------------------------------------------------- summary
Write-Host ''
if ($script:Fail -eq 0) {
    Write-Host "ALL PASS ($($script:Pass) checks)" -ForegroundColor Green
    exit 0
}
Write-Host "$($script:Fail) FAILED, $($script:Pass) passed" -ForegroundColor Red
$script:Fails | ForEach-Object { Write-Host "  - $_" }
exit 1
