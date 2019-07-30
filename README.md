# Hello DNS in Common Lisp

The canonical home page for this project is: https://git.sr.ht/~aerique/hello-dns-common-lisp

(The library is also pushed to GitLab and GitHub but those sites are not
monitored for support.)

A Common Lisp implementation of [Hello DNS](https://github.com/ahupowerdns/hello-dns/).

This document focuses on the Common Lisp issues.  Please refer to the
above link for the Hello DNS documentation and description.

## To Do

- Move RR parsing from `print-object` to object instantiation
    - (so we'll have `:A` as `rtype` and not `1`

## Known Issues

- SBCL focused, we should use compatibility libs
- DNS Tree and Compression: https://powerdns.org/hello-dns/tdns/README.md.html#parsingandgeneratingdnsmessages/dnsmessagewriter/compression
    - We do not do this yet since I do not grasp the DNS Tree and why
      this makes everything easier yet.
