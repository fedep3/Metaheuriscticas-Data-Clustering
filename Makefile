CC       := gcc
CFLAGS   := -O3 -Wall -c -msse2 -mpreferred-stack-boundary=4 -minline-all-stringops -mmmx -m3dnow
LDFLAGS  := -lstdc++ -ltiff
OBJFILES := $(patsubst src/%.cpp,obj/%.o,$(wildcard src/*.cpp))
RM       := rm -f

mhs: $(OBJFILES)
	$(CC) -o mhs $(OBJFILES) $(LDFLAGS)

obj/%.o: src/%.cpp
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) obj/*.o mhs

all: mhs
