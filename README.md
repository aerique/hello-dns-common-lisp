# Hello DNS in Common Lisp

A Common Lisp implementation of [Hello DNS](https://github.com/ahupowerdns/hello-dns/).

I seem to have found an okay level of abstraction for the first Common
Lisp version of Hello DNS.

## Known Issues

- SBCL focused, we should use compatibility libs
- DNS Tree and Compression: https://powerdns.org/hello-dns/tdns/README.md.html#parsingandgeneratingdnsmessages/dnsmessagewriter/compression
    - We do not do this yet since I do not grasp the DNS Tree and why
      this makes everything easier yet.
