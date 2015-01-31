fort = g++
flags = -Wall -g

prog = test

.cpp.o:
	$(fort) -c $(flags) $*.cpp

obj = connect4.o
#obj = testprog.o

all: clean jazz

clean:
	-rm *.o
	-rm $(prog).x

jazz: $(obj)
	$(fort) -o $(prog).x $(obj)