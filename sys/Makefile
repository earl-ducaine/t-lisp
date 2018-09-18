
OBJFILES = main.o sample.o tcall.o

test: $(OBJFILES)
	ld -o test /lib/crt0.o $(OBJFILES) -lm -lc

.s.o:
	/lib/cpp $*.s | as -o $@ -

sample.o: tmax.h
tcall.o: tmax.h

tmax.h:
