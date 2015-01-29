fort = ifort
flags = -g -O3 -W0 -fpp -axP -parallel -ip -132

prog = pig

.f.o:
	$(fort) -c $(flags) $*.f

obj = pig.o

all: clean jazz

clean:
	-rm *.o
	-rm $(prog).x

jazz: $(obj)
	$(fort) $(flags) $(obj) $(libs) -o $(prog).x
