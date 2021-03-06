# LVCA (pronounced "luca")

## Introduction

LVCA is a tool for building programming languages. It has an intentionally
small core. You create a language by specifying (1) its syntax, (2) its statics (ie typechecking rules), and (3) its dynamics (ie how it evaluates).

LVCA then provides tools:
* parser
* pretty-printer
* interpreter
* debugger

Things that don't yet exist but can and should:

* An automatic typechecker
  - I'd like to have at least two versions of this. Ideally, all of your typechecking rules are specified in a bidirectional style, which gives us an algorithm for typechecking. Failing that, rules might be specified in an ad-hoc way and solved via an SMT solver.
* Automatic serialization (to JSON, cbor, or some other format)
* Relatedly, content-identifiation
* Language-server protocol implementation

## Example

First we define the abstract syntax of the language. This simple language only
has booleans and functions.

```
tm :=
  // a term can be a simple boolean literal
  | true()
  | false()

  // ... or a type-annotated term (holding a tm and a ty)
  | annot(tm; ty)

  // ... an if-then-else
  | ite(tm; tm; tm)

  // ... or a function or function application. Note the `tm. tm` syntax means
  // that we bind a `tm` in the body of the function (also a `tm`). Contrast
  // with `tm; tm` which means there are two `tm` children.
  | lam(tm. tm)
  | app(tm; tm)

ty :=
  // a type is either a bool
  | bool()
  // or an arrow between two types
  | arr(ty; ty)
```

The syntax we're using here comes from Robert Harper's [Practical Foundations
for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl/).

Next we define the typechecking rules for this language. We support expressing
typing rules in a
[bidirectional](http://davidchristiansen.dk/tutorials/bidirectional.pdf) style.
I'd like to add support for unification in the future.

```

----------------------- (bool intro 1)
ctx |- true() => bool()

------------------------ (bool intro 2)
ctx |- false() => bool()

      ctx |- tm <= ty
-------------------------- (annot)
ctx |- annot(tm; ty) => ty

ctx |- t1 <= bool()  ctx |- t2 <= ty  ctx |- t3 <= ty
----------------------------------------------------- (bool elim)
           ctx |- ite(t1; t2; t3) <= ty

    ctx, x : ty1 |- tm <= ty2
---------------------------------- (lam intro)
ctx |- lam(x. tm) <= arr(ty1; ty2)

ctx |- tm1 => arr(ty1; ty2)  ctx |- tm2 <= ty1
---------------------------------------------- (lam elim)
        ctx |- app(tm1; tm2) => ty2

// important: this rule must go last or else it will subsume all others
ctx |- tm => ty
--------------- (switch)
ctx |- tm <= ty
```

Lastly, we define the denotational semantics of the language.

```
[[ true()          ]] = true()
[[ false()         ]] = false()
[[ annot(tm; ty)   ]] = [[ tm ]]
[[ ite(t1; t2; t3) ]] = case([[ t1 ]]; true() -> [[ t2 ]]; false() -> [[ t3 ]])
[[ lam(x. body)    ]] = lam(x. [[ body ]])
[[ app(fun; arg)   ]] = app([[ fun ]]; [[ arg ]])
```

Given all of these pieces, we can automatically produce an interpreter that
typechecks and evaluates expressions.

## Meaning of the name

1. LVCA is an acronym for Language Verification, Construction, and Automation

2. In biology, LUCA stands for Last Universal Common Ancestor -- the most recent common ancestor of all life on earth. LVCA occupies a somewhat analogous position as it can be used to implement any programming language.
