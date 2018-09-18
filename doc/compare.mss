A Biased Comparison of T and Common Lisp
(outline for a paper which might or might not get written one day)

Jonathan Rees
10 August 1984

- General info about T:
	- Analyzability
	- Simplicity
	- Philosophy 
		- Closures give power
		- Exploit the compiler
		- No global state
		- The "almost-functional language" approach

- General info about CL:
	- Zetalisp compatibility
		Many design decision were made based on the criterion
		that if made a certain way the decision could make an
		implementation of CL based on existing Symbolics
		software too difficult.
	- Features
		While both T and CL attempted to be conservative,
		there seem to be more features in CL than in T.
	- "Commonality, compatibility, stability"

- Similarities
	- "Portability, consistency, expressiveness, efficiency, power"
		T and CL share these goals, as articulated in the
		CL manual.
	- Default lexical scoping with optional dynamic binding
		This semantic decision, which agrees with Scheme and
		compiled Maclisp and Zetalisp, disagrees with Interlisp
		and interpreted Maclisp and Zetalisp.
	- Similar attitude towards special forms
		Minimal primitive special forms, others implemented as
		macros.  The idea that special forms must be known to
		a compiler and are really syntactic and semantic entities,
		whereas functions (procedures) aren't, contrasts with
		"old-style" Lisps, where as FEXPR is just one kind of
		function and the compiler is a kludge which might break
		your code.
	- Read syntax is nearly identical, except for package prefixes
	  	and various # things
	- Arithmetic, arrays, and basic data types are very similar
		(ratios, /, etc.)

- Miscellaneous incompatibilities
	- Names
		T's "?" convention vs. CL's "p"/"-p" convention
		T's "!" vs. CL's "n" prefix
		T's "->" vs. ad hoc
	- Function/variable space distinction
		CL: Need to use #' (even on lambda-exps) and FUNCALL
		T: One space, as in C, Algol, etc.
		(does this have anything to do with deep binding?)
	- Disposition of NIL
		CL: () is a symbol [thus complicating semantics (the types
		SYMBOL, LIST, and boolean all overlap), implementation
		(every routine dealing with symbols has to special-case
		()), and user code (e.g. () can't be used as a failure
		return for functions whose range includes symbols)]
		T: () is an object of its own type; there is a symbol with
		the name NIL which is different.  [Note: T could have gone
		further and made false different from the empty list.
		It didn't.  The NIL/() distinction is much less serious.]
	- Generalized assignment
		CL: DEFSETF [requires 10 pages to document; is a
		complicated syntactic transformation; frustrates renaming]
		T: SET is a trivial syntactic transformation; assignment
		routines are defined procedurally.  Leverage
		from closures and optimizing compiler.
	- Dynamic binding
		CL has SPECIAL declarations which affect the behavior
		of LAMBDA, LET, etc.
		The meaning of (LAMBDA (F) (FUNCALL F)) changes totally
		depending on whether F is special or not.  The programmer
		always knows; why not declare so at binding point?
		T has a special form BIND which does dynamic binding.
	- Multiple values
		In T, function call and return are symmetrical with
		respect to handling of multiple values.  In both CL and T,
		it is an error if too many arguments are passed to a
		function.  In CL, if too many values are returned to a
		continuation, the extra values are discarded.  This is
		what Interlisp does when too many arguments are passed
		to a function.  In T, the return protocol is checked in
		exactly the same way the call protocol is.
		[Multiple-value returns are not implemented in the released
		version of T, but are implemented in newer versions.]
	- Argument evaluation order
		L-to-R in CL (I think; can't find any statement one way
		or the other in the manual)
		T: order is indeterminate (as in Algol, C, BLISS)
	- Declarations
		CL: declarations are a part of the syntax of LAMBDA, LET,
		etc.
		T: declarations are modeled using imperative constructs
		(dead ends, i.e. unreachable points in code, signal
		compiler that optimizations are permissible)

- Environments/values vs. symbols/properties
		In general, T invests meaning in objects rather than
		symbols (names).  Association tables map names to objects
		which represent their meaning in various contexts.
	- Function values
		CL: Lists and symbols are applicable
		(FUNCALL '(LAMBDA ...) ...) works
		[Again, this complicates the semantics - the types PAIR and
		FUNCTION intersect, frustrating type-checking]
		T: procedure is a type disjoint from others
	- Packages
		CL uses two completely different binding mechanisms for
		lexical and global variables; an environment for the first,
		the symbol's value cell for the second.  [Subject to
		inconsistency problems; modules cannot be multiply
		instantiated; retargeting frustrated; dependence on global
		state, fuzziness of i/o behavior of compiler; etc.]
		T uses lexical scoping to implement a package (module)
		system/protected namespaces.  There is one symbol table,
		but a symbol can have different values in different
		environments.  [Not that this is without its problems.]
	- Syntax tables
		T distinguishes compile time and run time more strongly
		than does CL.  In CL, the binding of names to macros and
		special forms is handled through a mechanism similar to
		that of names to functions.  In T, totally separate
		association tables are used for compilation and execution,
		and they may be separately parameterized at compile time
		and run time.
	- Early binding
		An attempt is made in T to clarify the notion of early
		binding.  An early binding table, which gives a set of
		permissible assumptions about the values of free variables,
		is passed as an argument to the compiler.
	- Types
		Types are symbols or lists in CL.  [This inhibits lexical
		scoping of types, forcing all type names to be global.]
		In T, types are predicates (like SATISFIES in CL);
		unions etc. can be done using combinators like DISJOIN.
			CL		T
			'INTEGER	INTEGER?
			'(AND A B)	(CONJOIN A B)
	- Property lists
		T has tables, which form arbitrary associations between
		objects and can be properly controlled (scoped), not property
		lists, which form a global database.  (Two CL programs
		may be at odds over the global resource (GET 'CAR 'CAR).)

- Global state
	- CL: the behavior of the system is controlled by various
	  global resources, including the package name table,
	  symbol property lists, the value of *READTABLE*, and so on.
	  T: state is usually packaged in tables or other objects.
	  Read tables are associated with streams or passed as an 
	  argument to the reader.

- CL features missing from T
	- TAGBODY
		PROG (with GO) can be implemented in T as a macro
		which expands into LABELS, but such a macro
		is not, by default, part of T.  It easily could be.
	- optionals & keys
		This could also be implemented source-to-source, although
		an efficient implementation would be somewhat tricky.
	- math
		CL's math functions could be in any T implementation given
		sufficient manpower or portable enough code.  As a language
		feature, CL's math package ought to be incorporated
		into the definition of T (perhaps simply by reference).
	- sequences
		T is weak in handling sequence data types;
		selection, search, and iterators are implemented in an
		ad hoc per-type manner.  CL has a powerful set of generic
		operators.  An efficient implementation of these is not
		straightforward.  As language design it seemed somewhat
		random; a better facility seems possible, so this was
		not addressed in T.
	- bells & whistles: format ops, circular printing, etc.
		While the absence in T of many CL features can be annoying,
		"creeping featurism" seems to be an evil worth avoiding
		even if the price is a little painful.
		Are features a Good Thing?  Yes and no.
	- Features can be ported from CL to T, but usually not vice versa.
		Porting features from CL to T should be straightforward.
		Going the other way is harder because it is not given
		that CL supports closures efficiently.

- T features missing in CL
	- T's OBJECT-expressions provide:
		- abstract data types
		- an object-oriented programming system
		- a way to do things to procedures other than call them
	  Object-oriented programming was explicitly omitted from the CL
	  design because it is still an area where further experimentation
	  and research seems necessary.  It's not clear that T does it
	  the right way.  (But it sure is useful anyhow.)
	- GC hooks: populations, OBJECT-HASH
		These T features are amazingly useful for system-level
		hacking.  But they are properly considered experimental,
		and are therefore not appropriate for a conservative
		language design like CL's.
	- Upwards continuations: current T implementations don't have
	  these, but future ones probably will.  They are important.
	- Coroutines and tasks: similarly.

- Tractability (analyzability)
	- What does a meta-circular CL interpreter look like?
	  I haven't seen one, but I'd expect it to be complex.
	  A T interpreter is included in the T manual's appendix.
	  Scheme interpreters seem to be generally easier to write.

- Support
	- CL has two or three companies supporting it and the weight of
	  ARPA behind it.  The manual is slick (except for the code font's
	  digits).  The favored programming style is exposited in many
	  textbooks (Touretzky, Wilensky, Winston, etc.).
	- T's manual is dense, hard to read.  There are no educational
	  materials.  People at Yale will continue to support T, but
	  company support still looks unlikely.
	  Steele & Sussman wrote a series of very interesting
	  papers about Scheme, but these are not highly available.
	  Perhaps these could be anthologized one day.
	  The new textbook (Sussman & Abelson) is a big plus.  Scheme has
	  a small but growing and devoted following, including groups at
	  CMU, Indiana, Harvard, MIT, UCLA, Yale, and Texas Instruments.

- Philosophy
	- T: Lexical closures change the way you look at the world
	  (CL implementations of closures are inefficient).
	  The assumption that closures are "lightweight" is used to
	  advantage in the both the definition and the implementation of
	  the language.  The fact that CL has closures isn't going to make
	  a difference unless people can feel that they are well supported.
	- T: rely on compilers, not on users or language design, to
	  accomplish efficient implementation, to the extent possible.
