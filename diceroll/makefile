fort = ifort
flags = -g -O3 -W0 -fpp -axP -parallel -ip -132

# fort = gfortran
# flags = -O -ffixed-line-length-132

prog = euler

.f.o:
	$(fort) -c $(flags) $*.f

obj = diceroll.o

all: clean jazz

clean:
	-rm *.o
	-rm $(prog).x

jazz: $(obj)
	$(fort) $(flags) $(obj) $(libs) -o $(prog).x
