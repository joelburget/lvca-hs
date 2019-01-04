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

## Meaning of the name

1. LVCA is an acronym for Language Verification, Construction, and Automation

2. In biology, LUCA stands for Last Universal Common Ancestor -- the most recent common ancestor of all life on earth. LVCA occupies a somewhat analogous position as it can be used to implement any programming language.
