(HERALD TCENV)                  ; -*- Mode:T; System:TCOMP -*-

;;; Copyright (c) 1983 Yale University

;;; Establish an environment for creating the compiler.

(LET ((E (MAKE-LOCALE *SYSTEM-ENV* '*TC-ENV*)))
  (*DEFINE *SYSTEM-ENV* '*TC-ENV* E)
  (*DEFINE E '*ESTABLISH-TARGET-PARAMETERS?* T)
  (SET (*VALUE *T-IMPLEMENTATION-ENV* '*THE-BOOT-ENV*) E))
