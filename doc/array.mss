@comment{
	This section was excised from data.mss on 6/7/84.	-JR
}


@subsection[Array]

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-----------------------------+---------+-+---+---------+-+---+
|0|          N (= rank)         |vec type |1|imm| array'  |1|imm|
+-+-----------------------------+---------+-+---+---------+-+---+
|                         dimension N-1                         |
+---------------------------------------------------------------+
|                             ...                               |
+---------------------------------------------------------------+
|                          dimension 0                          |
+---------------------------------------------------------------+
|              total number of cells used (for GC)              |
+-+-----------------------------+---------+-+---+---------+-+---+
|0|          N (= rank)         |vec type |1|imm|  array  |1|imm| <--- ptr
+-+-----------------------------+---------+-+---+---------+-+---+
|                        array elements                         |
|                              ...                              |
+---------------------------------------------------------------+
]

    ``vec type'' specifies the element type, analogously to vectors.

    The following is an indexing scheme for the VAX, and it has the bug that
    the VAX is designed for 1-based arrays so the bounds checking is over-liberal
    by one in each dimension.  The only way to get around that is by doing one
    extra instruction (a decrement or a compare) for each dimension.

@begin[example]
(aref a0 j1 j2 j3)
 ==> (%aref a1 (+ (* (+ (* j1 (%adim a0 1)) j2) (%adim a0 2)) j3))

    CMPL -esc+1(A0),#<<3*256>+elem_type_etc>
    BNE error
    INDEX J1,#0,-esc-8(A0), -tag-12(A0),#0,T    ;check idx 0, multiply dim 1
    INDEX J2,#0,-esc-12(A0),-tag-16(A0), T,T    ;check idx 1, multiply dim 2
    INDEX J3,#0,-esc-16(A0),#1,          T,T    ;check idx 2
    op/elem_type ...,-esc+4(A0)[T],...
@end[example]

