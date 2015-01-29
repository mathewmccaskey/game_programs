fort = ifort
 
flags = -g -O3 -W0 -fpp -axP -parallel -ip -132

prog = neural_net

.f.o:
	$(fort) -c $(flags) $*.f

obj = tictactoe.o random.o

all: clean jazz

clean:
	-rm *.o
	-rm $(prog).x

jazz: $(obj)
	$(fort) $(flags) $(obj) -o $(prog).x
