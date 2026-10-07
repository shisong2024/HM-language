# build-tmp/probe-imp.ps1 -- adversarial probe for the implicit-parameters feature
# (dictionary passing: `implicit def`, `{o :: Ord a}` params, `let implicit`)
#
# ASCII ONLY.  Run from the workspace root:
#   $env:PSExecutionPolicyPreference='Bypass'
#   & build-tmp\probe-imp.ps1
#
# Every `chk` below asserts the behaviour the DESIGN asks for
# (TODO/16-implicit-parameters.md sec.9 A-M plus adversarial cases).
# So a FAIL here is a candidate bug, not a broken expectation -- chk prints the
# raw output of the failing case, and the case name says what was expected.
#
# Sections:
#   1. sec.9 acceptance list A-M           (expected: all pass)
#   2. scope frames / nearest match        (expected: all pass)
#   3. propagation / partial application   (expected: all pass)
#   4. records / ADTs / synonyms / strings (expected: all pass)
#   5. session: import / reload / rollback (expected: all pass)
#   6. CONFIRMED-BUG repros                (expected: FAIL today, names start BUG-)
#   7. known-finding re-checks             (reproduction status in comments)

[CmdletBinding()]
param(
    [string]$Exe = ''
)

$ErrorActionPreference = 'Stop'
# Make sure the temp program files land in <root>/build-tmp (run.ps1 uses the CWD).
$root = Split-Path $PSScriptRoot -Parent
if (Test-Path (Join-Path $root 'src/Interp/TypeCheck.hs')) { Set-Location $root }
if ($Exe -eq '') { $Exe = Join-Path $PSScriptRoot 'v1.exe' }
$env:PSExecutionPolicyPreference = 'Bypass'
. (Join-Path $PSScriptRoot 'run.ps1') -Exe $Exe

function Section([string]$t) {
    Write-Host ''
    Write-Host ('=== ' + $t + ' ' + ('=' * [Math]::Max(0, 60 - $t.Length)))
}

# ---- helper modules used by the import/reload/rollback cases -----------------
function Write-Mod([string]$name, [string]$text) {
    [System.IO.File]::WriteAllText((Join-Path $PSScriptRoot $name), $text,
        (New-Object System.Text.UTF8Encoding($false)))
}
# ordColor :: Ord M.Color, plus a polymorphic consumer
Write-Mod 'probe_mod_a.txt' @'
data Color = Red | Blue;
def mkOrd f = Ord { cmp = f };
implicit def ordColor = mkOrd (lambda a b -> EQ) :: Ord Color;
def pick a b = min a b;
'@
# a candidate that checks OK inside a batch that fails later -> must not be kept
Write-Mod 'probe_mod_bad.txt' @'
implicit def myStr = Ord { cmp = lambda a b -> GT } :: Ord String;
def broken = 1 :: String;
'@
# two further Ord Int candidates from modules (compete with Prelude.ordInt)
Write-Mod 'probe_mod_int.txt' @'
implicit def myInt = Ord { cmp = lambda a b -> GT } :: Ord Int;
'@
Write-Mod 'probe_mod_int2.txt' @'
implicit def otherInt = Ord { cmp = lambda a b -> GT } :: Ord Int;
'@

# =============================================================================
Section '1. sec.9 acceptance list (A-M)'
# =============================================================================

# A: unique concrete candidate, used at a concrete type -> auto-inserted.
$null = chk 'A-unique-concrete-candidate' accept 'Value: GT' @'
def mkOrd f = Ord { cmp = f };
implicit def ordI = mkOrd (lambda a b -> GT) :: Ord Int;
def use {o :: Ord Int} x = o.cmp x 0;
use 3;
'@

# A2: a definition may refer to a candidate defined LATER in the same batch
# (sec.6.2: the elaborated body must also be evaluable).
$null = chk 'A2-candidate-defined-after-use' accept 'Value: GT' @'
def mkOrd f = Ord { cmp = f };
def useI {o :: Ord Int} x = o.cmp x 0;
implicit def ordLate = mkOrd (lambda a b -> GT) :: Ord Int;
useI 3;
'@

# A3: a batch-local candidate shadows a session/prelude candidate of the same
# type (the batch frame is nearer than sImp).
$null = chk 'A3-batch-candidate-shadows-prelude' accept 'Value: 7' @'
def mkOrd f = Ord { cmp = f };
implicit def ordG = mkOrd (lambda a b -> GT) :: Ord Int;
min 5 7;
'@

# B: missing dictionary -> the error must name the required type.
$null = chk 'B-missing-names-type' reject 'Ord Color' @'
data Color = Red | Blue;
def useC {o :: Ord Color} x = o.cmp x x;
'@

# B2: missing dictionary at a call site (type determined by the argument).
$null = chk 'B2-missing-at-call-site' reject 'Ord (Box Int)' @'
data Box a = Box a;
def useB {o :: Ord a} x = o.cmp x x;
useB (Box 1);
'@

# C: two matching candidates in the same frame -> ambiguity naming BOTH, and no
# fallback to the outer (prelude) frame.
$null = chk 'C-same-frame-ambiguity-names-both' reject 'More than one implicit value matches Ord Int: `oa`, `ob`.' @'
def mkOrd f = Ord { cmp = f };
implicit def oa = mkOrd (lambda a b -> EQ) :: Ord Int;
implicit def ob = mkOrd (lambda a b -> LT) :: Ord Int;
min 5 7;
'@

# D: nearest matching frame wins (local Ord Int must beat Prelude.ordInt).
$null = chk 'D-nearest-frame-wins' accept 'Value: 7' @'
def mkOrd f = Ord { cmp = f };
let implicit loc = (mkOrd (lambda a b -> GT) :: Ord Int) in min 5 7;
'@

# D2: same, but inside a definition body (definition-site capture).
$null = chk 'D2-nearest-frame-in-def-body' accept 'Value: (7, 1)' @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit loc = (mkOrd (lambda a b -> GT) :: Ord Int) in (min 5 7, x);
f 1;
'@

# E: a local candidate that does NOT match must not shadow an outer matching one.
$null = chk 'E-unmatched-local-does-not-shadow' accept 'Value: 5' @'
def mkOrd f = Ord { cmp = f };
let implicit loc = (mkOrd (lambda a b -> GT) :: Ord String) in min 5 7;
'@

# E2: two nested local frames, the nearest of the two wins.
$null = chk 'E2-nested-local-frames' accept 'Value: 5' @'
def mkOrd f = Ord { cmp = f };
let implicit i1 = (mkOrd (lambda a b -> GT) :: Ord Int) in
  (let implicit i2 = (mkOrd (lambda a b -> LT) :: Ord Int) in min 5 7);
'@

# F: an unresolved constraint propagates outward through a call chain.
$null = chk 'F-constraint-propagates' accept 'Forall a0. {Ord a0} => (a0 -> (a0 -> a0))' @'
def f x y = min x y;
def g x y = f x y;
'@

# G: the presence of Ord Int must not specialise an ordinary type variable.
$null = chk 'G-candidate-does-not-specialise' accept 'def f : Forall a0. {Ord a0} => (a0 -> (a0 -> a0))' @'
implicit def ordG = Ord { cmp = lambda a b -> GT } :: Ord Int;
def f x y = min x y;
'@

# H: partial application / bare reference must not produce a fake ambiguity.
$null = chk 'H-bare-reference' accept 'def f : Forall a0. {Ord a0} => (a0 -> (a0 -> a0))' @'
def f = min;
'@

$null = chk 'H2-partial-application' accept 'def f : (Int -> Int)' @'
def f = min 1;
'@

# I: concrete definition-site capture (0 : Int fixes x's type inside the body, so
# no {Ord..} predicate survives and y stays polymorphic).
$null = chk 'I-definition-site-capture' accept 'def f : Forall a0. (Int -> (a0 -> Int))' @'
def f x y = min x 0;
'@

# J: two Wanted of the same type must become ONE hidden parameter.
$null = chk 'J-duplicate-wanted-dedup' accept 'Forall a0. {Ord a0} => (a0 -> (a0, a0))' @'
def f x = (min x x, max x x);
'@

# K: self-recursion works (the mutual-recursion form is BUG-2 in section 6).
$null = chk 'K-self-recursion-ok' accept 'Value: 5' @'
def f x = if true then x else min x (f x);
f 5;
'@

# L: module import makes the module implicit available (qualified name).
$null = chk 'L-import-candidate-used' accept 'Value: M.Blue' @'
import "build-tmp/probe_mod_a.txt" as M;
M.pick M.Red M.Blue;
'@

# L2: re-importing under the same alias must unload the old candidate
# (otherwise the frame would hold two M.ordColor and the use would be ambiguous).
$null = chk 'L2-reload-same-alias-no-dup' accept 'Value: M.Blue' @'
import "build-tmp/probe_mod_a.txt" as M;
import "build-tmp/probe_mod_a.txt" as M;
M.pick M.Red M.Blue;
'@

# L3: two modules exporting candidates for the same type -> ambiguity names them.
$null = chk 'L3-two-modules-ambiguous' reject 'otherInt' @'
import "build-tmp/probe_mod_int.txt" as I;
import "build-tmp/probe_mod_int2.txt" as J;
min 5 7;
'@

# M: a failed batch must not pollute the implicit frame.
# probe_mod_bad.txt declares Ord String (GT) and then an ill-typed def, so the
# whole batch rolls back; `min "a" "b"` must use Prelude.ordStr and must NOT see
# the failed myStr.  NOTE: the module's own failure prints "Type Error:" in the
# output, so this case is a `reject` + -NotNeedle (exactly the pollution symptom).
$null = chk 'M-failed-batch-no-pollution' reject 'Value: "a"' @'
import "build-tmp/probe_mod_bad.txt" as B;
min "a" "b";
'@ -NotNeedle 'More than one implicit value'

# M2: a candidate whose *own* definition fails must not be visible in that batch.
$null = chk 'M2-failed-candidate-not-visible' reject 'No implicit value of type Ord Color' @'
data Color = Red | Blue;
def mkOrd f = Ord { cmp = f };
implicit def badColor = 3 :: Ord Color;
def useC {o :: Ord Color} x = o.cmp x x;
'@

# =============================================================================
Section '2. scope frames, nearest match, let-implicit scoping'
# =============================================================================

$null = chk 'scope-truly-unmatched-local-keeps-outer' accept 'Value: (5, 1)' @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit s = (mkOrd (lambda a b -> GT) :: Ord String) in (min 5 7, x);
f 1;
'@

# a let implicit whose requirement is decided inside the body is captured.
$null = chk 'scope-local-captured-in-body' accept 'Value: 7' @'
def mkOrd f = Ord { cmp = f };
let implicit loc = (mkOrd (lambda a b -> GT) :: Ord Int) in (min 5 7);
'@

# the let-implicit binder is also an ordinary value in scope (no search needed).
$null = chk 'scope-local-used-explicitly' accept 'Value: (EQ, 1)' @'
def mkOrd f = Ord { cmp = f };
def f x = let implicit loc = (mkOrd (lambda a b -> EQ) :: Ord Int) in (loc.cmp 5 7, x);
f 1;
'@

# a let-bound implicit that itself needs a dictionary is rejected (doc 19 sec.4.2).
$null = chk 'scope-local-candidate-with-context-rejected' reject 'cannot be an implicit candidate' @'
let implicit o = min in min 1 2;
'@

# two implicit params of the same type share one dictionary (doc 19 sec.4.1)
$null = chk 'scope-two-same-type-params-one-dict' accept 'Forall a0. {Ord a0} => (a0 -> (Ordering, Ordering))' @'
def two {o :: Ord a} {p :: Ord a} x = (o.cmp x x, p.cmp x x);
'@

# two implicit params of different types must be passed in the right order.
$null = chk 'scope-two-types-two-dicts' accept 'Value: (EQ, EQ)' @'
def g {o :: Ord a} {p :: Ord b} x y = (o.cmp x x, p.cmp y y);
g 1 "a";
'@

# =============================================================================
Section '3. propagation / partial application'
# =============================================================================

$null = chk 'prop-through-two-levels' accept 'Value: 1' @'
def outer x y = min x y;
def caller a b = outer a b;
caller 1 2;
'@

$null = chk 'prop-through-lambda' accept 'Forall a0. {Ord a0} => (a0 -> (a0 -> a0))' @'
def f x = lambda z -> min x z;
'@

$null = chk 'prop-recursive-lambda-let' accept 'def f : Int' @'
def f = let o = lambda y -> min y y in o 1;
f;
'@

$null = chk 'prop-dedup-across-nested-lambda' accept 'Forall a0. {Ord a0} => (a0 -> (a0 -> (a0, (a0 -> a0))))' @'
def f x y = (min x y, lambda z -> min x z);
'@

$null = chk 'prop-no-specialisation-at-callsite' reject 'No implicit value of type Ord Color' @'
def mkOrd f = Ord { cmp = f };
data Color = Red | Blue;
implicit def cStr = mkOrd (lambda a b -> EQ) :: Ord String;
def f x = min x x;
f Red;
'@

# =============================================================================
Section '4. records / ADTs / synonyms / strings'
# =============================================================================

$null = chk 'types-string-candidate' accept 'Value: "a"' @'
min "a" "b";
'@

$null = chk 'types-record-adt-candidate' accept 'Value: EQ' @'
def mkOrd f = Ord { cmp = f };
data Wrap a = Wrap a;
implicit def wInt = mkOrd (lambda a b -> EQ) :: Ord (Wrap Int);
def useW {o :: Ord (Wrap Int)} = o.cmp (Wrap 1) (Wrap 2);
useW;
'@

$null = chk 'types-synonym-candidate' accept 'Value: EQ' @'
type ColorO = Ord Int;
implicit def oi2 = Ord { cmp = lambda a b -> EQ } :: ColorO;
def useI {o :: Ord Int} x = o.cmp x x;
useI 3;
'@

$null = chk 'types-implicit-param-used-as-field' accept '{Ord a0} => (a0 -> (a0 -> Ordering))' @'
def f {o :: Ord a} x y = o.cmp x y;
'@

# =============================================================================
Section '5. session state: import / rollback (multi-batch via file)'
# =============================================================================

# a failed batch must not leave its candidate behind: after the failed module a
# fresh batch-local Ord Int candidate still wins over Prelude.ordInt.
# (again `reject` + -NotNeedle: the module's own error is in the output)
$null = chk 'session-failed-batch-then-normal' reject 'Value: 7' @'
import "build-tmp/probe_mod_bad.txt" as B;
def mkOrd f = Ord { cmp = f };
implicit def mine = mkOrd (lambda a b -> GT) :: Ord Int;
min 5 7;
'@ -NotNeedle 'More than one implicit value'

# module candidate survives into the importing batch and is usable qualified.
$null = chk 'session-module-candidate-persists' accept 'Value: M.Blue' @'
import "build-tmp/probe_mod_a.txt" as M;
M.pick M.Red M.Blue;
'@

# =============================================================================
Section '6. CONFIRMED BUG repros (these are expected to FAIL today)'
# =============================================================================

# BUG-1 (let-bound generalisation eats the requirement).
# sec.1.1/1.4: `let y = min x in y` inside a polymorphic def must give
#   def f : Forall a0. {Ord a0} => (a0 -> a0)
# and `f 3 4` must evaluate to 3.  Today the definition is REJECTED with
# "Cannot determine the type required by implicit parameter Ord a0."
# Cause: inferLet (TypeCheck.hs:679-703) calls closeBinding with env1, which
# contains the enclosing function's parameters, so genVars excludes x's type var
# and closeBinding's `propagates` check (:807-821) fails.
$null = chk 'BUG-1-let-binding-rejects-propagating-wanted' accept 'Value: 3' @'
def f x = let y = min x in y;
f 3 4;
'@

# BUG-1b: the same defect through a let-bound lambda.
$null = chk 'BUG-1b-let-lambda-rejects-propagating-wanted' accept 'Value: 3' @'
def outer x = let g = lambda y -> min x y in g x;
outer 3;
'@

# BUG-2 (SCC does not share constraints; elaboration leaves a free dictionary).
# sec.6.1: every reachable member of the SCC gets the constraint and the program
# must run.  Today: f gets `a -> a` (no constraint at all!), g gets
# `{Ord a} => a -> a`, and f's elaborated body calls `g $implicit0 3` where
# `$implicit0` is never bound -> Eval Error at run time.
# Cause: sccCheckerElab.checkOne (:299-331) builds recDicts from each binding's
# own hiddenNames but closes each binding with only its OWN wanted list, so the
# caller never gets the hidden parameter its body already references.
$null = chk 'BUG-2-scc-leaves-unbound-hidden-dict' accept 'Value: 3' @'
def f x = g x;
def g x = if true then min x x else f x;
f 3;
'@

# BUG-2b: the same program's public type for f is unsound (f is accepted at any
# type even though its body needs an Ord dictionary).
$null = chk 'BUG-2b-scc-unsound-public-type' accept 'def f : Forall a0. {Ord a0} => (a0 -> a0)' @'
def f x = g x;
def g x = if true then min x x else f x;
'@

# BUG-2c: the same defect turns into variable capture when the callee's hidden
# dictionary was named after its implicit parameter: `substRecCalls` inserts
# `Var "o"` for the dictionary, the caller's own `let o = 0` captures it, and the
# dictionary slot is filled with the integer 0.  f's public type again claims no
# constraint, so the program type-checks and dies at run time with a bogus
# message ("Pattern match is not exhaustive: no arm matches 0.").
$null = chk 'BUG-2c-scc-hidden-dict-captured-by-local' accept 'Value: EQ' @'
def f x = let o = 0 in g x;
def g {o :: Ord a} x = if true then o.cmp x x else f x;
f 5;
'@

$null = chk 'BUG-2c2-scc-captured-dict-unsound-type' accept 'def f : Forall a0. {Ord a0} => (a0 -> Ordering)' @'
def f x = let o = 0 in g x;
def g {o :: Ord a} x = if true then o.cmp x x else f x;
'@

# BUG-3 (phantom predicate makes a function uncallable).
# A Wanted whose type variable occurs nowhere in the function type is kept as a
# predicate: `def f : Forall a0 a1. {Ord a0} => (a1 -> a1)`.  No caller can ever
# discharge `Ord a0`, so `f 3` is rejected although no dictionary is needed.
# Cause: inferDeclBody (:906-916) creates the Wanted with the *annotation*'s
# freshened type, which no use of the parameter has to unify with; closeBinding's
# genVars (:807-812) then generalises that unused var into a predicate.
$null = chk 'BUG-3-phantom-predicate-uncallable' accept 'Value: 3' @'
def f {o :: Ord a} x = let d = o in x;
f 3;
'@

# BUG-4 (wrong error kind for an undetermined requirement at top level).
# sec.5.C/8: a requirement that is neither concrete nor generalisable must report
# `Cannot determine the type required by implicit parameter ...`.  Inside a
# definition the checker says exactly that, but a top-level expression instead
# reports "No implicit value ... is available" for a type that cannot even be
# named (Ord a0) -- telling the user to add an instance that can never match.
# Cause: programChecker (:221-234) resolves every Wanted with resolveWanted
# directly, bypassing the isClosedType / AmbiguousImpType classification that
# closeBinding applies.
$null = chk 'BUG-4-toplevel-open-wanted-wrong-error' reject 'Cannot determine the type required by implicit parameter' @'
min;
'@

# BUG-5 (misleading message for a polymorphic candidate).
# `Forall a0. Ord [a0]` has NO `=>` context, but the error says it "still
# requires Forall a0. Ord [a0]." -- the real reason is that the candidate is
# polymorphic / not closed.  The diagnostic also carries no span (no caret),
# while the local `let implicit` form of the same error does have one.
# Cause: Pretty.hs (:130-132) prints ImpCandidateHasContext's whole scheme after
# the word "requires", and programChecker (:208-212) raises it with
# `Located Nothing`.
$null = chk 'BUG-5-polymorphic-candidate-message' reject 'candidate is polymorphic' @'
implicit def ordl {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
'@

# BUG-6 (missing span for a def-site missing dictionary).
# sec.8: the diagnostic must carry the target type AND a source position.  A
# concrete requirement that fails inside a definition is reported with no
# snippet at all (no `2 | ...` / caret line), so a body with several implicit
# requirements gives no clue which one failed.
# Cause: inferDeclBody (:916) builds the Wanted with `wtdSpan = Nothing`, and
# nothing later fills it in (the StmtExpr path gets spans from At nodes; the
# definition path does not).
$null = chk 'BUG-6-missing-span-at-def-site' accept '| def useC' @'
data Color = Red | Blue;
def useC {o :: Ord Color} x = o.cmp x x;
'@

# BUG-7 (duplicate implicit parameter names silently accepted).
# `{o :: Ord Int} {o :: Ord String}` binds `o` twice: the later binder wins in
# the body, the earlier requirement becomes a phantom, and the public type
# `(String -> Ordering)` silently drops it.  There is no duplicate-parameter
# check (cf. DuplicatePatVar / DuplicateDef).
# Cause: inferDeclBody (:893-916) M.inserts each param without a duplicate check.
$null = chk 'BUG-7-duplicate-implicit-param-names' reject 'duplicate parameter' @'
def dup {o :: Ord Int} {o :: Ord String} x = o.cmp x x;
'@

# =============================================================================
Section '7. known-finding re-checks (reproduction status)'
# =============================================================================

# known #1: a candidate with a free type variable is rejected (correct), but the
# message claims there is a context.  Same defect as BUG-5.
$null = chk 'known-1-polymorphic-candidate-rejected' reject 'cannot be an implicit candidate' @'
implicit def ordl {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
'@

# known #2 first half: `def a {o :: Ord a} = o;` -> {Ord a0} => Ord a0. REPRODUCES.
$null = chk 'known-2a-param-annotation-is-the-context' accept 'Forall a0. {Ord a0} => Ord a0' @'
def a {o :: Ord a} = o;
'@

# known #2 second half: the "unused implicit parameter" claim.  On this build the
# exact program gives the CLEAN `Forall a0. Ord [a0]` -- the requirement
# disappears entirely instead of becoming `{Ord a0} => Ord [a1]`.  An *inert* use
# of the parameter is what manufactures the disjoint-variable phantom (known-2c).
$null = chk 'known-2b-unused-param-vanishes-clean' accept 'def a2 : Forall a0. Ord [a0]' @'
def a2 {o :: Ord a} = Ord { cmp = lambda x y -> EQ } :: Ord [a];
'@

# known #2 variant: annotation vars stay disjoint when the parameter is used
# inertly -> the scheme carries an extra var and a phantom dictionary (BUG-3 root).
$null = chk 'known-2c-disjoint-annotation-vars' accept 'def a2b : Forall a0 a1 a2. {Ord a0} => (a1 -> (Ord a0, a1, Ord [a2]))' @'
def a2b {o :: Ord a} x = (o, x, Ord { cmp = lambda p q -> EQ } :: Ord [a]);
'@

# known #3: self-referential dictionary  {Ord [a0]} => Ord [a0]. REPRODUCES.
$null = chk 'known-3-self-referential-dict' accept 'Forall a0. {Ord [a0]} => Ord [a0]' @'
def a3 {o :: Ord a} = Ord { cmp = lambda x y -> o.cmp x y } :: Ord [a];
'@

# doc 19's call-site dictionary syntax is explicitly not implemented in v1.
$null = chk 'doc19-callsite-dict-not-implemented' reject 'Parse Error' @'
def mkOrd f = Ord { cmp = f };
implicit def ordI = mkOrd (lambda a b -> EQ) :: Ord Int;
min implicit { o = ordI } 1 2;
'@

$null = Show-Summary
