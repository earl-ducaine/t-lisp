@make[article]

@style[spacing .9]
@modify[itemize, spacing .9]
@modify[description, spacing .9]
@modify[enumerate, spacing .9]
@style[indent 0]
@style[bibselect cited]
@style[references = STDalphabetic]
@style[time = "4:30 pm"]
@style[date = "8 March 1952"]

@set[page=1]
@define[diagram=ProgramExample, above 1, longlines keep]
@define[example=ProgramExample, above 1, longlines keep]
@define[tc=t]

    @heading[
Data representation in T version 3
             ]

    @blankspace[2 lines]

    @center[ Jonathan Rees ]
    @center[ @value[date] @value[time] ]

    @blankspace[2 lines]

    Informal document!  Very sketchy.  Don't take too seriously.

    This document describes T3's data representations from the
    point of view of an implementor or garbage collector. 

    The following considerations have constrained the design of the data
    representations:
    @begin[itemize]
	Runtime typing: the ability to pass a descriptor for any object,
	and to determine its actual type at some later time, is basic.
	The representation of these descriptors must be optimized both for type
	testing and for dereferencing.
	
	Garbage collection: the subfields of any structure must be
	identifiable as either ``clean'' (rootable) or ``dirty'' (ignorable).
	
	Scannable heaps: it must be possible to scan through memory and
	determine which locations hold pointers and which ones don't.

	Lightweight lexical closures.

	Lightweight coroutines and tasks.

	Pure, position-independent, dynamically loadable code.
    @end[itemize]

    All concrete numbers (cell width, etc.) are only suggestions.  They
    have the 68000 and VAX in mind, but they are provided mostly as a
    visualization aid.  Most of this is general enough to be applicable to
    machines with different cell size and addressing granularity (e.g.
    the Titan or DEC-20).

@section[Terminology]

    Type terminology, where not indigenous, is from Common Lisp.

    @i[Object] is a primitive semantic concept.  The term is used in the
    same sense as in the manual.

    A @i[descriptor] is a bit pattern of a fixed size (e.g. 32 bits) which
    denotes or refers to an object.  Descriptors contain or point to
    both data and type information.  (Traditionally the term @i[pointer]
    has been used, but this is confusing because descriptors may only
    contain immediate data, and not actually point to anything.)

    The term @i[cell] is used to refer to the amount of storage needed to
    hold a descriptor.  (For the VAX and 68000, a cell is a
    longword.  For the Titan or DEC-20, a cell is a word.)

    Two objects are @i[eq] iff the bit patterns for their descriptors
    are identical.

    A @i[continuation] is a stack frame.

    A @i[slice] is a portion of an array - a section, plane, rectangle,
    etc.  A slice shares storage with the array of which it is a slice.
    (The term is from Algol 68.)

@section[Descriptor]

    Descriptors look like this:

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-----------------------------------------------------------+---+
|                           stuff                           |tag|
+-----------------------------------------------------------+---+
]

    Depending on the value of the ``tag'' field, ``stuff'' may be either
    a pointer into memory or immediate data.  In the first case, the object 
    which the descriptor describes is called a @i[stored object].
    In the second case, the object is an @i[immediate object].

    On byte-addressed machines, the tag field can be two bits (with
    cell-aligned data) or three bits (with doublecell-aligned data).  On
    cell-addressed machines, we can get by with only one type bit (and
    doublecell alignment), for the immediate versus stored distinction;
    if the object is immediate, then other bits may further determine
    the type, and if it's stored, then one can always tell the type by
    examining the first cell at the address pointed to.

    If there are two tag bits, the four type codes could distinguish:
    small integer, other immediate object, pair, and other stored object.
    These tag values are notated in the diagrams as ``fix,'' ``imm,''
    ``pr,'' and ``sto.''

    [Need more discussion.  What extra tag bits do and don't do for you.]

@section[Small integer]

    Small integers are also known as ``fixnums,'' although the latter is
    an archaic and confusing term.  They are represented by descriptors
    which contain the two's complement representation of the integer
    directly.

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------------------+---+
|S|                     the integer                         |fix|
+-+---------------------------------------------------------+---+
]

    S = sign bit.

@section[Immediate]

    If the tag is immediate, then its type is further determined by a
    subtype field in the descriptor.

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-----------------------------------------------+---------+-+---+
|                  immediate data               | subtype |N|imm|
+-----------------------------------------------+---------+-+---+
]

    Immediate data includes short floating-point numbers, characters, and
    immediate headers (see below).

    N = number bit.  Set when this descriptor either is a number or
    is the header of a stored object which is a number.

@subsection[Short float]

    24 bits of the descriptor itself specify a less-than-single-precision
    floating point number.  On the Apollo, these bits can be
    adjoined with 8 zero bits to get a valid IEEE single-precision
    floating point number.  On the VAX, things are much trickier because
    of the losing floating-point representation.  However, a @tc[ROTL]
    instruction will probably do the trick:

@example[
    BICL3 #^xFF,SRC,DEST
    ROTL #16,DEST,DEST
]

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------+-------------+---------------+---------+-+---+
|S|   exponent    |  msb -->    |      --> lsb  | shortflt|1|imm|  (VAX)
+-+---------------+-------------+---------------+---------+-+---+
]

@subsection[Character]

    Hack for @tc[TEXT-ELT]:

@example[
    MOVZWL 1-TAG(TEXT),A1
    MOVB #CHAR,A1
]

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-------------------------------+---------------+---------+-+---+
|               0               |   ASCII code  |   char  |0|imm|
+-------------------------------+---------------+---------+-+---+
]

@subsection[Immediate header]

    The first cell of any non-pair stored object must be a descriptor for
    some kind of @i[header].  Headers may be either immediate objects
    or stored objects.  If the header is an immediate object, then
    the header descriptor's subtype field determines the type of the
    stored object, e.g. vector, number, or symbol.

    In the case of a vector, the header's data field contains the length
    of the object in which it's contained.  See below.

@section[Stored object]

    The descriptor points to a chunk of memory; the type is further determined
    by examining the first cell in the chunk.  The descriptor value in this
    first cell is the object's @i[header].  A header may be either
    an @i[immediate header] or a @i[template].

    If the header is immediate, then the stored object is a vector or a
    stored number or is of some other primitive object type.  If the header
    is a template (a template itself being a kind of stored object),
    then the stored object is a @i[closure].  Templates and closures are
    described below.

    On the 68000, a decriptor may also be some 32-bit value whose
    high bit is set, in which case the stored object is itself a template.
    This is because there is no way to control what the value of the
    low bits of the first cell of a template will be.

@subsection[Simple vector]

    If the header is immediate and is one of the 16 vector header types,
    then the object is a simple vector of some sort:

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|               number of elements            |vec-type |N|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                        the elements                           |
|                            ...                                |
+---------------------------------------------------------------+
]

where vec-type describes the type of the vector's elements:
@itemize[
    integer, cross product of
        {8-bit, 16-bit, 32-bit}
        and {unsigned, signed}.

    1-bit-unsigned (i.e., a bit)

    char

    general

    single-float

    double-float
]

or else says that this is a large integer (N = 0), not a vector:
@itemize[
    positive large-integer  (number)

    negative large-integer  (number)
]

    The sign bit of the size needs to be zero for different reasons on
    the VAX and 68000.  The VAX has no LSH instruction, so the size would
    have to be extracted using EXTZV instead of ASHL (less efficient).
    On the 68000, the vector could otherwise be mistaken 
    for a template: we have no control over what the low two
    bits of the header word of a template are going to be,
    because they are part of the template's instruction stream.
    On the 68000 we assume that any non-pair stored object whose header 
    has the sign bit set is a template.

    Vectors with more than 2@+[23]-1 = 8,388,607 elements must be
    represented using the more general multi-dimensional array mechanism.

    The following sections give examples of some of the vector types.

@paragraph[Bit vector]

    In Common Lisp terminology, a bit vector is a ``simple vector of bit.''
    Bit vector is abbreviated @i[bitv].

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|             number of bits                  |  bitv   |0|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                         the bits                              |
|                           ...                                 |
+---------------------------------------------------------------+
]

    Bit vectors larger than 1 megabyte cannot be represented.

@paragraph[String text]

    (Simple vector of string-char)

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|              number of characters           |  text   |0|imm| <--- ptr
+-+-------------+---------------+---------------+---------+-+---+
|     char      |     char      |     char      |     char      |
+---------------+---------------+---------------+---------------+
|                           ...                                 |
+---------------------------------------------------------------+
]

    Strings larger than 8 megabytes cannot be represented.

@paragraph[General vector]

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|             number of elements              | general |0|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                         element 0                         |tag|
+-----------------------------------------------------------+---+
|                           ...                             |   |
|                                                           |   |
+-----------------------------------------------------------+---+
]

    Vectors larger than 32 megabytes cannot be represented (except as
    arrays).

@paragraph[Large integer]

    Also known as ``bignums.''

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+-+-------+-+---+
|0|             number of binary digits         |S|bignum |1|imm| <--- ptr
+-+---------------------------------------------+-+-------+-+---+
|                         the digits                            |
|                            ...                                |
+---------------------------------------------------------------+
]

    Large integers are practically identical to bit vectors.  To the GC
    they are identical.

    S = sign bit.  Most Lisp implementations store large integers
    as signed-magnitude, but Glenn Burke decided that large integers
    in NIL should be stored two's-complement.
    Signed-magnitude makes multiplication and division work out nicer,
    but bitwise logical operations are a horror; two's-complement
    is just the opposite.  T's current integer arithmetic package
    uses signed-magnitude, so we'll probably stick with that for a while.

@subsection[Closure]

    Here the term @i[closure] means a procedure, @tc[(OBJECT ...)] instance,
    structure, or stack frame.  In general, a closure is any stored object
    whose header is a template.

    The size information, and anything else (number of arguments,
    etc.), is found by examining the closure's template.

@paragraph[Heaped closure]

    A heaped closure occurs as an independent stored object in a heap.
    The closure's size is found in the closure's template.  The storage
    following the template consists of some number of descriptors (these
    have tags and are therefore rootable) and some number of other
    cells containing arbitrary non-rootable data (bit fields,
    integers, floating point numbers, or whatever).

@diagram[
+-+---------------------------------------------------------+---+
|0|          address of template with H = 1, I = 0          |sto| <--- ptr
+-+---------------------------------------------------------+---+
|                    descriptor cells ...                   |   |
|                              ...                          |   |
+-----------------------------------------------------------+---+
|                      scratch cells ...                        |
|                                                               |
+---------------------------------------------------------------+
]

@paragraph[Closure-internal closure]

    A closure may be internal to another closure.  In this case its template
    is necessarily a bit-vector-internal template whose I-bit is set.
    The template's size information fields gives the closure's offset within
    its containing object.

@diagram[
|                ... enclosing environment ...              |   |
+-+---------------------------------------------------------+---+
|0|          address of template with H = 0, I = 1          |sto| <--- ptr
+-+---------------------------------------------------------+---+
|                ... enclosing environment ...              |   |
]

    In T 2 these are called ``tprocs'' (trivial procedures).

@paragraph[Stacked closure]

    Usually these are continuations, but they might be other things as well.
    See diagram in the section on stacks; the area marked ``the closure''
    is a stacked closure.

@subsection[Template]

    A @i[template] is a stored object which acts as a header for a closure.
    Any closure is said to be an @i[instance] of its template.  Many
    closures may share the same template.  The template is the static
    portion of the closure, and contains size information for the
    instances as well as code to run when an instance is called or
    when a generic operation is applied to the instance.

    A template whose instances are continuations is called a @i[return
    point].

    Templates come in several varieties, but they all contain the following
    information.  The information is of two sorts: that which describes
    the template per se, and that which describes all stored objects
    whose header is this template.

68000:
@diagram[
+---------------+---------------+-------------------------------+
|  # ptr cells  |  # scr cells  |  offset within containing obj |
+-+-+-+---------+---------------+-------------------------------+
|1|H|I| unused  |   # of args   |  instructions ---->>          | <--- ptr
+-+-+-+---------+---------------+                               |
|								|
]

VAX:
@diagram[
+---------------+---------------+-------------------------------+
|  # ptr cells  |  # scr cells  |  offset within containing obj |
+---------------+---------------+---------------+-+-+-----+-+---+
|          <<---- instructions  |   # of args   |H|I|tmplt|0|imm| <--- ptr
|                               +---------------+-+-+-----+-+---+
|								|
]

@begin[description]
offset within containing object
    @\The offset (in addressing units (bytes)! - templates for return
    frames aren't necessarily aligned) of this template within its
    containing object (bit vector or closure).  This is mostly for the GC,
    so that it can move the bit vector or closure in which this template
    is contained.

# of args
@\@multiple[If >0, then the instance is a procedure; this field is then the
           expected number of arguments in a call to the instance,
           plus one.

    If =0, then either it's ``n-ary'' and will do its own number of
           args checking, or it's not callable at all, and will complain.

    If <0, instance is a continuation (stack frame), and this field is the
           number of return values expected, minus one.  (Since usually
           only one return value is expected, this field is -2 for most
           return points.)
]

I @\If set, then each instance is internal to another object (probably
    a closure).  The
    instance's offset within its containing object is found by adjoining
    the ``# ptr cells'' and ``# scratch cells'' fields and treating them
    as one 16-bit integer.

H @\If set, then the instance is ``heaped'' and not ``stacked.''  If the
    instance is ``stacked,'' then the GC must move it with its containing
    stack,
    which is found by tracing frames outwards until a ``stack base''
    (vector) is found.  [This is necessary only if stacks may be contained
    within active heaps, probably not a desirable feature.]

# ptr cells, # scratch cells
  @\If the I bit is clear, then this gives size information for
    instances.  The first is the number of cells which contain descriptors,
    and must therefore be traced by the GC; the second is the number
    of additional cells which must not be rooted from.
@end[description]

@paragraph[Bit-vector-internal template]

    If the first two bytes of the instruction stream are @i[not] the ``jump
    absolute'' opcode, then the template is necessarily internal
    to a bit vector, and it has a handler jump displacement
    field.

68000:
@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
|                              ...                              |
|           instruction stream for previous template            |
|                               +-------------------------------+
|                               |  handler's jump displacement  |
+---------------+---------------+-------------------------------+
|  # ptr cells  |  # scr cells  |    offset within bit vector   |
+-+-+-+---------+---------------+-------------------------------+
|1|H|I| unused  |   # of args   |  instructions ---->>          | <--- ptr
+-+-+-+---------+---------------+                               |
|                      instruction stream                       |
|                              ...                              |
]

VAX:
@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
|                              ...                              |
|           instruction stream for previous template            |
+-------------------------------+                               |
|  handler's jump displacement  |                               |
+---------------+---------------+-------------------------------+
|  # ptr cells  |  # scr cells  |   offset within bit vector    |
+---------------+---------------+---------------+-+-+-----+-+---+
|          <<---- instructions  |   # of args   |H|I|tmplt|0|imm| <--- ptr
|                               +---------------+-+-+-----+-+---+
|                      instruction stream                       |
|                              ...                              |
]

@begin[description]
handler jump displacement
  @\Add this value to the location of the first instruction to obtain the
    point to jump to in order to invoke a generic operation on the instance.
@end[description]

    Bit-vector-internal templates correspond to the ``code chunks'' of
    T version 2.

@paragraph[Closure-internal template]

    Sometimes templates are internal not to bit vectors, but to closures.
    This is the normal case for templates which head closures which
    are not ``top-level'' procedures.

    When operation dispatch is performed on an instance which has a
    closure-internal template, the template's ``auxiliary template'' is
    fetched, and control is passed to the address for that template's
    handler.

    68000:
@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
|                              ...                          |   |
|                    enclosing environment                  |   |
+---------------+---------------+---------------------------+---+
|  # ptr cells  |  # scr cells  |    offset within closure  | 0 |
+-+-+-+---------+---------------+---------------------------+---+
|1|H|I| unused  |   # of args   |0 1 0 0 1 1 1 0 1 1 1 1 1 0 0 1| <--- ptr
+-+-+-+---------+---------------+---------------------------+---+
|              address of auxiliary template                |sto|
+-----------------------------------------------------------+---+
|                    enclosing environment                  |   |
|                              ...                          |   |
]
    The low two bytes of the first cell (4EF9) begin the templates's
    instruction stream.  4EF9 is the jump absolute opcode, so jumping
    to the template will in turn cause control to go to its auxiliary
    template (assuming the stored object type tag is 2).

    VAX:
@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
|                              ...                          |   |
|                   enclosing environment                   |   |
+---------------+---------------+---------------------------+---+
|  # ptr cells  |  # scr cells  |   offset within closure   | 0 |
+---------------+---------------+---------------+-+-+-----+-+---+
|1 0 0 1 1 1 1 1|0 0 0 1 0 1 1 1|   # of args   |H|I|tmplt|0|imm| <--- ptr
+---------------+---------------+---------------+-+-+-----+-+---+
|              address of auxiliary template                |sto|
+-----------------------------------------------------------+---+
|                   enclosing environment                   |   |
|                              ...                          |   |
]
    The high two bytes of the template's first cell (9F17) begin
    the template's instruction stream.  See remark above.

    Closure-internal templates correspond to what @i[all] templates are in
    T version 2.  The other two kinds of template are new.

@paragraph[Dynamic template]

    Templates can be created on the fly for newly-invented structure and
    closure types.  Dynamic templates are just like closure-internal
    templates.  The auxiliary template is a special ``trampoline'' which shifts
    the arguments right and inserts the structure as the first argument,
    then calls the structure type handler (?).  The handler for the auxiliary
    template does something analogous to make operations work.

    68000:
@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-----------------------------------------------------------+---+
|                 the-dynamic-template-header               |tag|
+---------------+---------------+---------------------------+---+
|  # ptr cells  |  # scr cells  |              -2           | 0 |
+-+-+-+---------+---------------+---------------------------+---+
|1|H|I| unused  |   # of args   |0 1 0 0 1 1 1 0 1 1 1 1 1 0 0 1| <--- ptr
+-+-+-+---------+---------------+---------------------------+---+
|                 address of trampoline code                |sto|
+-----------------------------------------------------------+---+
|                     descriptor closure                    |sto|
+-----------------------------------------------------------+---+
]

    VAX:
@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-----------------------------------------------------------+---+
|                 the-dynamic-template-header               |tag|
+---------------+---------------+---------------------------+---+
|  # ptr cells  |  # scr cells  |             -2            | 0 |
+---------------+---------------+---------------+-+-+-----+-+---+
|1 0 0 1 1 1 1 1 0 0 0 1 0 1 1 1|   # of args   |H|I|tmplt|0|imm| <--- ptr
+-------------------------------+---------------+-+-+-----+-+---+
|                 address of trampoline code                |sto|
+-----------------------------------------------------------+---+
|                     descriptor closure                    |sto|
+-----------------------------------------------------------+---+
]

@subsection[Stack]

    This diagram would be inverted on machines whose stacks grow towards
    higher memory.

@diagram[
+-+---------------------------------------------+---------+-+---+
|0|                 (unused)                    |  stack  |0|imm| <--bot-N
+-+---------------------------------------------+---------+-+---+
|                                                               |
|                              ...                              |
+-----------------------------------------------------------+---+
|                        top of stack                       |tag| <--- SP
+-----------------------------------------------------------+---+
|                              ...                              |
|                 next (newer) stacked closures                 |
+-+---------------------------------------------------------+---+<--\
|0|          address of template with H = I = 0             |sto| <-|- ptr
+-+---------------------------------------------------------+---+   |
|                       descriptor cells                    |   |   | the
|                              ...                          |   |   | clo-
+-----------------------------------------------------------+---+   | sure
|                         scratch cells                         |   |
|                              ...                              |   |
+-+---------------------------------------------------------+---+<--/
|0|           previous (older) stacked closures             |sto|
|                              ...                              |
+-+---------------------------------------------+---------+-+---+
|0|  789ABC (or any easily recognizable value)  | bottom  |0|imm| <--- bot
+-+---------------------------------------------+---------+-+---+
|               size of stack = N                           |fix|
+-----------------------------------------------------------+---+
|            current size = SP - bot                        |fix|
+-----------------------------------------------------------+---+
|                       parent pointer                      |sto|
+-----------------------------------------------------------+---+
|                coroutine state block pointer              |sto|
+-----------------------------------------------------------+---+
]

    The coroutine state block is an object which contains all the ``process
    state'' information enumerated in the file ``call.doc'', including all the
    rest-argument registers (?).  The state pointer and the SP-bot fields
    are garbage if this stack is the currently running one.  The state
    pointer is nil if the coroutine suspended itself voluntarily
    (synchronously).

@section[List]

    If tag bits are in short supply (i.e. there is only one), then
    lists may be represented just like any two-element structure.
    That is, the descriptor for the list points to three cells in memory,
    where the first is a header and the other two are for the car and cdr:

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|                  (unused)                   |   pair  |0|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                       the list's cdr                      |tag|
+-----------------------------------------------------------+---+
|                       the list's car                      |tag|
+-----------------------------------------------------------+---+
]

    An optimization on this is to declare that any stored object whose
    first cell is not a header is interpreted as a pair.

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-----------------------------------------------------------+---+
|                       the list's cdr                      |tag| <--- ptr
+-----------------------------------------------------------+---+
|                       the list's car                      |tag|
+-----------------------------------------------------------+---+
]

    The order of the car and cdr is immaterial.

    When a heap scan reaches the end of one object and wants to know the type
    of the next, it looks at the first cell.  If that cell contains a
    header, then the header determines the type and size of the
    object.  If it is not a header, then the object is a pair.

    A type check ought to be performed every time a pair's first cell (cdr)
    is set.  This doesn't really impose a restriction on
    users, however, since headers are not released objects - there are no
    released procedures which create, expose, or manipulate headers.

    Runtime type test: an object is a pair if its descriptor's tag
    says that the object is stored, and the first cell at the location
    pointed to is not a header.  This test might be expensive, so one
    would prefer either to use the three-cell representation or
    or, if there is more than one tag bit, to encode the type in the tag.

@subsection[Pair]

    Pointer to two cells which contain the car and cdr of the cell.
    An object is a pair if it is a list and it's not eq to @tc[()].

@subsection[Empty list]

    Pointer to @tc[()], whose car and cdr are @tc[()].

@section[Miscellaneous]

    Other types introduce no new complexity from the garbage
    collector's point of view.

@subsection[Single float]

    The descriptor points to two cells in memory.  The first is a decriptor
    identifying that the object pointed to is indeed a float, and the
    next contains the bits of the number.

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|                  (unused)                   | single  |1|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                 32-bit floating-point number                  |
+---------------------------------------------------------------+
]

@subsection[Double float]

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|                unused                       | double  |1|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                   64-bit floating-point number                |
|                                                               |
+---------------------------------------------------------------+
]

@subsection[String slice]

    In T 2, these are known as ``string headers'' or ``strings''.

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|                   length                    |  slice  |0|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                  pointer to string text                   |sto|
+-----------------------------------------------------------+---+
|                slice's offset within text                     |
+---------------------------------------------------------------+
]

@subsection[Array]

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|                   (unused)                  |  array  |0|imm| <--- ptr
+-+---------------------------------------------+---------+-+---+
|                   pointer to rank vector                  |sto|
+-----------------------------------------------------------+---+
|                  pointer to element vector                |sto|
+-----------------------------------------------------------+---+
]

More generally, one could fairly easily have arbitrary array slices
a la Algol 68.

@subsection[Symbol]

    Symbols want to be pure and position independent, and therefore
    don't want to contain any pointers.

    This makes it impossible to do @w[@tc[SYMBOL->STRING]] without consing, but
    that seems like a reasonable price to pay.  Writing
    @w[@tc[STRING->SYMBOL]]
    might be a little rough because some primitive would have to be
    invented to compare the characters in a string slice against
    those in a symbol.

    @tc[GET] and @tc[PUT] are a little complicated also.  It will be
    necessary to take the ``two-dimensional sparse table'' idea seriously.
    If symbols aren't allowed to point to property lists then the
    association will have to be kept in an independent table.
    The main disadvantage is that T won't do well on property-intensive LISP
    benchmarks.  The advantage is that you'll be able to GET/PUT to
    anything, not just symbols.  Also, the globality (timesharing)
    problem is solved because multiple property tables are possible.

@diagram[
   3                   2                   1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+---------------------------------------------+---------+-+---+
|0|         length of symbol text               |  symbol |0|imm| <--- ptr
+-+-------------+---------------+---------------+---------+-+---+
|     char      |     char      |     char      |     char      |
+---------------+---------------+---------------+---------------+
|                           ...                                 |
+---------------------------------------------------------------+
]


@section[Compiled expression]

    Object file (compiled expression) format.  No new primitive
    types are needed.

    A compiled expression is a structure consisting of:
    @itemize[
        Compiled code aggregate (bit vector)

        Environment descriptor (structure)

        Annotation information
    ]

    An environment descriptor is a pair:
    @itemize[
        Vector of symbols, quoted constants, and special immediate objects
        (primops)

        Bit vector driving the environment instantiation routine
    ]

    An environment descriptor may be constructed out of a bit vector read
    from a file.  The bit vector is scanned; an opcode (cell) is fetched
    and interpreted, and succeeding cells are snarfed out of the bit vector
    in an opcode-specific way.  Opcodes direct the constructor to put
    various kinds of things in the descriptor vector:

    @itemize[
         Symbols with a given print name

         Immutable strings with a given text

         Pairs, vectors, etc.
    ]

    Annotation information gives names of lexical variables, error
    condition information, etc.
