LLVM_ROOT=/usr/local
LIBDIR=../lib

CXX=g++
CFLAGS=-O2 -g -Wall -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -fomit-frame-pointer -fvisibility-inlines-hidden -fno-exceptions -fno-rtti -Woverloaded-virtual -Wcast-qual

#LDFLAGS=-Wl,--version-script,default.exports.map  -lpthread -ldl -lm
LDFLAGS= -lpthread -ldl -lm

objs = $(patsubst %.cpp,%.so,$(wildcard *.cpp))

all: $(objs)

%.o: %.cpp
	$(CXX) $(CFLAGS) -fPIC -I$(LLVM_ROOT) -c $< -o $@

%.so: %.o
	mkdir -p $(LIBDIR)
	$(CXX) $(CFLAGS) -shared $< -o $(LIBDIR)/$@ $(LDFLAGS)

clean:
	rm -f *.o
