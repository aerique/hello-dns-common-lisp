# Hello DNS in Common Lisp

A Common Lisp implementation of [Hello DNS](https://github.com/ahupowerdns/hello-dns/).

## Branches

### `feature/do-not-convert-keywords`

This branch does not convert bytes / bits / integers in a DNS message to
meaningful keywords in Common Lisp.  While the latter makes debugging
easier it also makes serialization of a DNS message harder and also
requires that we add extra functions for converting the keywords back
to "DNS message values".

Now the meaning of the values will only be shown when printing a class.

This is something I want to try out and am not sure if it is a good idea.
