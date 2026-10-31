"""End-to-end checks. Usage: python test/autoimp-regression.py path/to/interp.exe.
Run from the project root; fixtures/logs are written to build-autoimp/feature-cases.
"""
from pathlib import Path
import sys, subprocess, json

exe = Path(sys.argv[1]).resolve()
folder = Path('build-autoimp/feature-cases')
folder.mkdir(parents=True, exist_ok=True)
(folder/'library.txt').write_text('def compareTwice x y=(x<y,y<x);',encoding='utf8')
eq = "def eq=Ord {cmp'=lambda x y->EQ}::Ord Int;\n"
lt = "def lt=Ord {cmp'=lambda x y->LT}::Ord Int;\n"
cases = [
    ('closed-sharing', eq+'def h x y=(x<1,y<1); h @{ord0=eq} 2 3;',
     ['{ord0 :: Ord Int}', 'Value: (false, false)'], True),
    ('default-search', 'def h x y=(x<1,y<1); h 0 2;',
     ['Value: (true, false)'], True),
    ('independent-types', eq+"def st=Ord {cmp'=lambda x y->LT}::Ord String; def g x y z w=(x<y,z<w); g @{ord0=eq,ord1=st} 2 1 \"z\" \"a\";",
     ['ord0 :: Ord a0, ord1 :: Ord a1','Value: (false, true)'], True),
    ('slots-stable-after-specialization',eq+lt+'def g x y z w=(x<y,z<w); g @{ord0=eq,ord1=lt} 2 1 2 1;',
     ['Value: (false, true)'],True),
    ('explicit-distinct',eq+lt+'def f {left::Ord Int} {right::Ord Int} x=((<) @{o=left} x 1,(<) @{o=right} x 1); f @{left=eq,right=lt} 2;',
     ['left :: Ord Int, right :: Ord Int','Value: (false, true)'],True),
    ('explicit-name-reserved',eq+'def f {ord0::Ord Int} x y=(ord0.cmp\' x x,y<y); f @{ord0=eq,ord1=eq} 1 2;',
     ['ord0 :: Ord Int, ord1 :: Ord a0','Value: (EQ, false)'],True),
    ('different-callee-names-merge',eq+'def a {first::Ord Int} x=first.cmp\' x x; def b {second::Ord Int} x=second.cmp\' x x; def f x=(a x,b x); f @{ord0=eq} 1;',
     ['def f : {ord0 :: Ord Int}','Value: (EQ, EQ)'],True),
    ('alias-uses-new-interface',eq+'def f=min; f @{ord0=eq} 1 2;',
     ['Value: 2'],True),
    ('alias-merges-callee-slots',lt+'def u {a::Ord Int} {b::Ord Int} x=(a.cmp\' x x,b.cmp\' x x); let f=u in f @{ord0=lt} 1;',
     ['Value: (LT, LT)'],True),
    ('local-let',eq+'def f x=let cmpLocal=lambda y->x<y in cmpLocal x; f @{ord0=eq} 1;',
     ['Value: false'],True),
    ('strict-do',eq+'def f x=do {g=lambda y->x<y; g x}; f @{ord0=eq} 1;',
     ['Value: false'],True),
    ('recursive-auto-interface',lt+'def f n=if n==0 then 1<2 else f @{ord0=lt} (n-1); f 3;',
     ['def f : {ord0 :: Ord Int}','Value: true'],True),
    ('mutual-recursion',lt+'def a n x=if n==0 then x<x else b (n-1) x; def b n x=if n==0 then x<x else a (n-1) x; a @{ord0=lt} 3 1;',
     ['Value: true'],True),
    ('explicit-recursive-interface',lt+'def f {chosen::Ord Int} n=if n==0 then chosen.cmp\' 1 2 else f @{chosen=lt} (n-1); f 2;',
     ['chosen :: Ord Int','Value: LT'],True),
    ('independent-ability-counters','def f x y=(x<x,show y); f 1 true;',
     ['ord0 :: Ord a0, show0 :: Show a1','Value: (false, "true")'],True),
    ('search-name-is-not-slot-name',"implicit def ord0=Ord {cmp'=lambda x y->LT}::Ord Int; implicit def another=Ord {cmp'=lambda x y->GT}::Ord Int; def f x=x<1; f 0;",
     ['More than one implicit value matches'],False),
    ('wrong-type',"def f x=x<1; f @{ord0=showInt} 1;",
     ['Ord Int','Show Int'],False),
    ('unknown-slot','def f x=x<1; f @{ord9=ordInt} 1;',
     ['Unknown dictionary argument','ord0'],False),
    ('duplicate-slot','def f x=x<1; f @{ord0=ordInt,ord0=ordInt} 1;',
     ['duplicate named dictionary argument'],False),
    ('effect-preserved',eq+'def f x=do {putStrLn "effect-marker"; x<1}; f @{ord0=eq} 2;',
     ['{Console}', 'effect-marker','Value: false'],True),
    ('generic-list', 'def f xs=map show xs; f [1,2]; f [true];',
     ['show0 :: Show a0','Value: ["1", "2"]','Value: ["true"]'],True),
    ('imported-interface',f'import library.txt as L\n'+eq+'L.compareTwice @{ord0=eq} 1 2;',
     ['Value: (false, false)'],True),
    ('implicit-dependency-chain', 'ability Render a=Render {render::a->String}; def renderText {s::Show a} x=show @{s=s} x; implicit def r=Render {render=renderText}::Render a; def use {r::Render a} x=r.render x; use 1; use true;',
     ['Value: "1"','Value: "true"'],True),
]

results={}
for name, source, expected, success in cases:
    path=folder/(name+'.txt');path.write_text(source,encoding='utf8')
    r=subprocess.run([str(exe),path.as_posix()],capture_output=True,timeout=30)
    text=(r.stdout+r.stderr).decode('utf8',errors='replace').replace('\r','')
    (folder/(name+'.log')).write_text(text,encoding='utf8')
    ok=(r.returncode==0)==success and all(s.lower() in text.lower() for s in expected)
    results[name]=ok
    print(('PASS ' if ok else 'FAIL ')+name,flush=True)
    if not ok:print(text,flush=True)
repl = eq+lt+'def u {a::Ord Int} {b::Ord Int} x=(a.cmp\' x x,b.cmp\' x x);\n:t u\nu @{a=eq,b=lt} 1;\n:q\n'
r=subprocess.run([str(exe)],input=repl.encode('utf8'),capture_output=True,timeout=30)
text=(r.stdout+r.stderr).decode('utf8',errors='replace').replace('\r','')
(folder/'repl.log').write_text(text,encoding='utf8')
results['repl-interface-and-query']=r.returncode==0 and text.count('a :: Ord Int, b :: Ord Int')>=2 and 'Value: (EQ, LT)' in text
print(('PASS ' if results['repl-interface-and-query'] else 'FAIL ')+'repl-interface-and-query',flush=True)
if not results['repl-interface-and-query']:print(text,flush=True)
(folder/'summary.json').write_text(json.dumps(results,indent=2))
print(f'{sum(results.values())}/{len(results)} passed')
sys.exit(0 if all(results.values()) else 1)
