CC       := gcc
CFLAGS   := -O3 -Wall -c
LDFLAGS  := -lstdc++ -ltiff -lm -pg
OBJFILES := $(patsubst src/%.cpp,obj/%.o,$(wildcard src/*.cpp))
RM       := rm -f

mhs: $(OBJFILES)
	$(CC) -o mhs $(OBJFILES) $(LDFLAGS)

obj/%.o: src/%.cpp
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) obj/*.o mhs

all: mhs
