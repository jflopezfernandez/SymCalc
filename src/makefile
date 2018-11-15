
CC := ghc

CFLAGS := -g -Werror 

all: calc.exe

calc.exe: main.o
	$(CC) -o $@ $^

main.o: main.hs
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -rf *.o *.exe
