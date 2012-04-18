GHC      := ghc
HFLAGS   := --make
RM       := rm -f

runTests: src/*.hs
	$(GHC) $(HFLAGS) -o runTests src/*.hs
	rm -f src/*.o src/*.hi

clean:
	rm -f src/*.o src/*.hi runTests

all: mhs
