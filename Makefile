FC=gfortran
FCFLAGS=-g -fcheck=all -fbounds-check -ffpe-trap=invalid -fbacktrace -fdump-core -ffree-line-length-0 -cpp 
#LDFLAGS=-lncurses -lgfortran # dynamic
LDFLAGS=-static -lncurses -ltinfo -lgpm -lgfortran # static
OBJS=ncurses.o global.o levelmod.o fudge.o

%.o: %.f90
	$(FC) -c $(FCFLAGS) -o $@ $<

%.mod: %.o
	@if [ ! -f $@ ]; then rm $< ; $(MAKE) $< ; fi

all: fudge

clean:
	rm -f core *.mod *.o 

veryclean:
	rm -f core *.mod *.o fudge

fudge: $(OBJS)
	$(FC) -o fudge $(OBJS) $(LDFLAGS)

fudge.o levelmod.o global.o: ncurses.mod
fudge.o levelmod.o: global.mod
fudge.o: levelmod.mod
