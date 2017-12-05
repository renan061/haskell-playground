# 
# LCI - Lambda Calculus Interpreter
# 

all: clean
	@- ghc -o lci Main.hs

run: all
	@- ./lci

test:
	@- ghc -o tester Tester.hs
	@- ./tester

clean:
	@- rm -f *.hi
	@- rm -f *.o
	@- rm -f lci
	@- rm -f tester
