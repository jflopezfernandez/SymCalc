
CC := ghc

CFLAGS := -g -Werror

all: calc.exe

calc.exe: main.o Lexer.o Parser.o Evaluator.o
	$(CC) -o $@ $?

main.o: Main.hs
	$(CC) $(CFLAGS) -o $@ $^
	
Lexer.o: Lexer.hs
	$(CC) $(CFLAGS) -o $@ $^

Parser.o: Parser.hs
	$(CC) $(CFLAGS) -o $@ $^

Evaluator.o: Evaluator.hs
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -rf *.hi *.o *.exe
