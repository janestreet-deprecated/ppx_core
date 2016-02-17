Ppx\_core - a PPX standard library
==================================

Ppx\_core is a standard library for OCaml AST transformers. It
contains:

- various auto-generated AST traversal using an open recursion scheme
- helpers for building AST fragments
- helpers for matching AST fragments
- a framework for dealing with attributes and extension points

When used in combination with
[ppx\_driver](http://github.com/janestreet/ppx_driver), it features:

- spellchecking and other hints on misspelled/misplaced attributes and
  extension points
- checks for unused attributes (they are otherwise silently dropped by
  the compiler)
