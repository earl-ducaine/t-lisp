# Makefile for (TCOMP UTC)

as = /t/bin/tas               # Assembler
NEWGEN = /t/adm/newgen
TSYS = /t/t2.9.x/sys
TCOMP = .
OBJFILES = $(TSYS)/boot.o $(TSYS)/vaxslink.o $(TSYS)/vaxkernel.o \
	$(TSYS)/unxeno.o $(TSYS)/early.o $(TSYS)/reloc.o $(TSYS)/chunk.o \
	$(TSYS)/populate.o $(TSYS)/operation.o $(TSYS)/open.o $(TSYS)/pcommon.o \
	$(TSYS)/primops.o $(TSYS)/vaxprim.o $(TSYS)/vaxopen.o $(TSYS)/unix.o \
	$(TSYS)/stdio.o $(TSYS)/unfault.o $(TSYS)/unload.o $(TSYS)/vaxgc.o \
	$(TSYS)/ungc.o $(TSYS)/unfile.o $(TSYS)/untime.o $(TSYS)/vmwrite.o \
	$(TSYS)/vmrepl.o $(TSYS)/signal.o $(TSYS)/list1.o $(TSYS)/throw.o \
	$(TSYS)/fault.o $(TSYS)/frame.o $(TSYS)/gc.o $(TSYS)/gctop.o \
	$(TSYS)/vmsystem.o $(TSYS)/vector.o $(TSYS)/character.o $(TSYS)/string.o \
	$(TSYS)/hash.o $(TSYS)/misc.o $(TSYS)/struct.o $(TSYS)/pool.o \
	$(TSYS)/carcdr.o $(TSYS)/list2.o $(TSYS)/tree.o $(TSYS)/aliases.o \
	$(TSYS)/property.o $(TSYS)/table.o $(TSYS)/ratio.o $(TSYS)/arith.o \
	$(TSYS)/bignum.o $(TSYS)/dispatch.o $(TSYS)/syntax.o $(TSYS)/env.o \
	$(TSYS)/load.o $(TSYS)/eval.o $(TSYS)/sort.o $(TSYS)/debug.o \
	$(TSYS)/crawl.o $(TSYS)/lapconst.o $(TSYS)/recognize.o $(TSYS)/readtable.o \
	$(TSYS)/read.o $(TSYS)/print.o $(TSYS)/pfloat.o $(TSYS)/format.o \
	$(TSYS)/pp.o $(TSYS)/fs.o $(TSYS)/strops.o $(TSYS)/streams.o \
	$(TSYS)/channel.o $(TSYS)/standard.o $(TSYS)/repl.o $(TSYS)/object.o \
	$(TSYS)/macros.o $(TSYS)/let.o $(TSYS)/cond.o $(TSYS)/backquote.o \
	$(TSYS)/location.o $(TSYS)/defstruct.o $(TSYS)/export.o $(TSYS)/obsolete.o \
	$(TSYS)/exports.o $(TSYS)/tsystem.o $(TCOMP)/tcenv.o $(TCOMP)/tctop.o \
	$(TCOMP)/machax.o $(TCOMP)/node.o $(TCOMP)/defs.o $(TCOMP)/random.o \
	$(TCOMP)/sets.o $(TCOMP)/envinit.o $(TCOMP)/transduce.o $(TCOMP)/compile.o \
	$(TCOMP)/alphatize.o $(TCOMP)/cmacros.o $(TCOMP)/analyze.o $(TCOMP)/optimize.o \
	$(TCOMP)/subst.o $(TCOMP)/copy.o $(TCOMP)/strategy.o $(TCOMP)/annotate.o \
	$(TCOMP)/closure.o $(TCOMP)/targetize.o $(TCOMP)/pack.o $(TCOMP)/generate.o \
	$(TCOMP)/gmax.o $(TCOMP)/common.o $(TCOMP)/emit.o $(TCOMP)/output.o \
	$(TCOMP)/lap.o $(TCOMP)/tcsystem.o $(TCOMP)/vaxgen.o $(TCOMP)/vaxemit.o \
	$(TCOMP)/vaxsup.o $(TCOMP)/unemit.o $(TSYS)/uvmbase.o $(TCOMP)/utc.o \
	$(TSYS)/unassist.o /lib/crt0.o
SYSTEM = xutc

$(SYSTEM): Makefile
	$(NEWGEN) $(TSYS)/uvm $(TSYS)/ut $(TCOMP)/utc 
	$(as) -o $(TCOMP)/utc.o $(TCOMP)/utc.s
	ld -x $(OBJFILES) -ltermcap -lm -lc -e start
	mv a.out $(SYSTEM)
