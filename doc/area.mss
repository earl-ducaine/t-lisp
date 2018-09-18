@comment[  excised form data.mss on 6/7/84 - too random]


@section[Areas]

    Given any address, it is possible to determine, efficiently, what
    area contains it, but the only place this is really important is in
    the GC (and in operation dispatch, perhaps...).

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+---------------+---------------+---------------------------+---+
| level 1 index | level 2 index |   address within segment  |tag|
+---------------+---------------+---------------------------+---+
]

    Entries in segment tables are objects called ``areas''.  An area,
    obviously, represents a region of storage and type information
    about it.  This type information may include:

    @itemize[
         is this an active area with respect to GC?  That is, if a pointer
         into this area is encountered, does the object pointed to need
         to be copied?  If so, to what companion area should it be copied?

         the kind of object in the area: either first-class or one of the
         second-class types

         the immutability of the area - are cells created here mutable?
    ]

    There are two kinds of object, first-class and second-class.

    The type of any first-class object may be determined by examining the
    object itself and/or the pointer to it, without first consulting the
    segment table.

    Second-class objects include:

    @itemize[
         stack frames

         compiled code objects for return points

         foreign routines and pointers
    ]
    and must be boxed before being passed out to arbitrary code, because
    first-class predicates return indeterminate values for them.

    However, with respect to the GC, these pointers are safe in the sense
    that the GC cannot become confused by them (why?).
