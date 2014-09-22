CXX=clang++
CXXFLAGS=-g -c -std=c++11 -Wall -Wextra -pedantic -DNOEXCEPT_SUPPORTED
LDFLAGS=-g /usr/lib/x86_64-linux-gnu/libboost_regex.a

all: chess.out

chess.out: Main.o BoardRep.o
	$(CXX) -o chess.out Main.o BoardRep.o $(LDFLAGS)

Main.o: BoardRep.h Utility.h

BoardRep.o: BoardRep.h Utility.h
