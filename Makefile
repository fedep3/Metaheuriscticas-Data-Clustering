CC       := gcc
CFLAGS   := -c -g
LDFLAGS  := -lstdc++ -ltiff -lm
OBJFILES := $(patsubst src/%.cpp,obj/%.o,$(wildcard src/*.cpp))
RM       := rm -f

mhs: $(OBJFILES)
	$(CC) -o mhs $(OBJFILES) $(LDFLAGS)

obj/%.o: src/%.cpp
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) obj/*.o mhs

all: mhs
