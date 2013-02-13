CC       := gcc
CFLAGS   := -O3 -Wall -c -msse2 -mpreferred-stack-boundary=4 -minline-all-stringops -mmmx -m3dnow `Magick++-config --cxxflags --cppflags --ldflags --libs`
LDFLAGS  := -lstdc++ `Magick++-config --cxxflags --cppflags --ldflags --libs`
OBJFILES := $(patsubst src/%.cpp,obj/%.o,$(wildcard src/*.cpp))
RM       := rm -f

athena: $(OBJFILES)
	$(CC) -o athena $(OBJFILES) $(LDFLAGS)

obj/%.o: src/%.cpp
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) obj/*.o athena

all: athena
