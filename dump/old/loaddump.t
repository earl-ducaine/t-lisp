(herald load (env t))

(import *t-implementation-env* pointer->integer integer->pointer
        string-hash pointer-address pointer-tag flonum-guts
        fixnums-replace-flonum bignum? make-bytev bytev?
        bytev-length bref-8 *bytev-template*)
(load "//sigma/t/t3/orbit/dump/para.bin")
(load "//sigma/t/t3/orbit/dump/hashmap.bin")
(load "//sigma/t/t3/orbit/dump/encode.bin")
(load "//sigma/t/t3/orbit/dump/decode.bin")
