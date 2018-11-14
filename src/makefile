
CC := ghc

CFLAGS := -O

all: calc.exe

calc.exe: main.o
	$(CC) -o $@ $^

main.o: main.hs
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -rf *.o *.exe
