# clj-libstd

Misc standard library extensions. So far only tagged unions.

## Usage

This library provides two macros, `defunion' and `match'.

The following declaration introduces a union with three constructors,
T, V, and W.

    (defunion Tree
      T
      (V x xs)
      (W x xs))

The following case analysis returns `nil' or `(cons x xs)' or throws a
"badmatch"-Exception if val has tag `W'. If val is not a Tree, the
match-statement crashes.

    (match val
      T nil
      (V x xs) (cons x xs))

## License

Copyright jakob@primat.es 2013

Distributed under the new BSD license.
