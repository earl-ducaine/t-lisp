(herald loaddump)

;;; The dumper requires string tables.

;;; Load the three dump files into the implementation env.

(import *t-implementation-env*
        bytev?
        bytev-length
        bref-8
        *bytev-template*
        bignum?
        flonum-guts
        fixnums-replace-flonum)

(require (hax stringtable))

(load '(dump codes))
(load '(dump dump))
(load '(dump retrieve))
