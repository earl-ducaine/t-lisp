            .globl p_foo
            .data
p_foo:
            .long t_foo
            .long p_make_pair
            .long p_make_extend
            .text
t_foo:

p_4:
    movl    $8,r6
    movl    -2(r9),r0
    jsb     0(r0)
    moval   loop_11-2,2(r4)
    movl    r1,-2(r4)
    movl    r4,r0

p_6:
    movl    r1,r2
    movl    $1<<2,r1
    moval   2(r0),r0
    moval   0(sp),sp
    movl    $3,r10
    movl    -2(r0),r9
    jmp     (r9)

p_10:

loop_11:
    pushl   r1
    pushl   r2
    pushl   r0
    pushal  c_22

p_30:
    movl    $0<<2,r6
    cmpl    r2,r6
    bneq    p_34

p_32:
    movl    true,r1
    moval   0(sp),sp
    movl    $-2,r10
    movl    (sp)+,r9
    jmp     (r9)

p_34:
    movl    r11,r1
    moval   0(sp),sp
    movl    $-2,r10
    movl    (sp)+,r9
    jmp     (r9)

c_22:
    cmpl    r1,nil
    beql    p_16

p_15:
    movl    8(sp),r1
    moval   12(sp),sp
    movl    $-2,r10
    movl    (sp)+,r9
    jmp     (r9)

p_16:
    movl    4(sp),r6
    subl2   $1<<2,r6

c_24:
    movl    4(sp),r7
    mull2   8(sp),r7

c_26:
    movl    r6,r2
    movl    r7,r1
    movl    0(sp),r0
    moval   -2(r0),r0
    moval   12(sp),sp
    movl    $3,r10
    movl    -2(r0),r9
    jmp     (r9)
