# 
# Haskell
# 

run: clean
	@- ghc -fno-warn-tabs -o executable $(file)".hs"
	@- ./executable

clean:
	@- rm -f *.hi
	@- rm -f *.o
	@- rm -f executable