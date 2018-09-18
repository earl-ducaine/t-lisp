@make[article]
@device[imprint10]

@set[page=1]

@heading[Why a New T Compiler?]

Short-term gains.

    Correctness of compiled code.

        Many code generation bugs.

        Environment screwups.

    Efficiency (resource usage) issues.

        Arguments in registers, and better code generally.

        Startup time.

            Scannable heaps imply easy SUSPEND.

        Frequency of garbage collection.

            Stack-allocated objects for evaluator, dynamic binding, and
            rest-arguments.

        Speed of compilation.

            Simpler, less redundant compiler.

            No separate assembly step.

        Size of system.

            Code size will diminish if object code is tighter.

            Data representations will be more compact.

Long-term gains.

    Maintainability and modifiability.

    Retargetability.

        Titan, Pyramid, Ridge, 801

    Language features.  (Any of these could be done right now, but not
    efficiently.)

        New data types: arrays, bit sets, etc.

        Multiple-value returns.

        Optional arguments.

        JOIN.

        Module interconnect.

    Common Lisp implementation.

Possible objections.

    Compatibility with current T is a primary goal.

The plan.

    Goal is not only to have a compiler, but a documented one.
    It should be possible for someone who is not a T expert to bring
    it up on the machine of his choice.
    A retargeting document will therefore be essential.

    Orbit is in 3rd month of development.

        Most design problems already solved; structure in place.
        Some internal design documents exist.

        Rees is working on front end and closure analysis.

        Kranz is working on register allocation.

        Adams is working on code emission and machine descriptions.

    Timetable (1984):

        Basics in place by June 1: most primops, closures, system
        generation, GC.

        Read-eval-print loops ("VM system") built by Sept 1.

        T system available to general users by Dec 1.

    People:

        Rees, Adams, Kranz, Philbin, Shivers, Kelsey, Li

    Potential DEC arrangement.
