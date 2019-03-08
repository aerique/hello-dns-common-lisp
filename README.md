# Hello DNS in Common Lisp

A Common Lisp implementation of [Hello DNS](https://github.com/ahupowerdns/hello-dns/).

I'm having a very hard time to find the right balance between staying
close to the original C++ Hello DNS source and being lispy.  This means
the source currently looks a little schizophrenic.

## Branches

### `feature/do-not-convert-keywords`

This branch does not convert bytes / bits / integers in a DNS message to
meaningful keywords in Common Lisp.  While the latter makes debugging
easier it also makes serialization of a DNS message harder and also
requires that we add extra functions for converting the keywords back
to "DNS message values".

Now the meaning of the values will only be shown when printing a class.

This is something I want to try out and am not sure if it is a good idea.

Also, this branch will try to stay closer to the original Hello DNS.
This means the Common Lisp will sometimes read like transliterated C++.
