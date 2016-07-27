CXX ?= clang++
CXXFLAGS ?= -std=c++1z -Wall -Wextra -pedantic -O2

.PHONY: all clean

all: .DIR

.DIR: gendir
	./gendir

%: %.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $^ -o $@

clean:
	rm .DIR gendir
