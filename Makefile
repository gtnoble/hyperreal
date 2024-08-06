CSC = csc
CSC_FLAGS = -O3 -debug o -verbose
CSC_SO_FLAGS = $(CSC_FLAGS) -s
CSC_BIN_FLAGS = $(CSC_FLAGS)

.PHONY:all

all: hyperreal.test

.PHONY:test

test: hyperreal.test
	./hyperreal.test

.PHONY:libs

libs: hyperreal.test.so hyperreal.so

.PHONY:clean

clean:
	rm *.so *.test

%.so %.types: %.scm types.so.types types.scm
	$(CSC) $(CSC_SO_FLAGS) -prologue types.scm -emit-types-file $(basename $<).so.types $<

hyperreal.test: hyperreal.test.scm hyperreal.so hyperreal.so.types
	$(CSC) $(CSC_BIN_FLAGS) -consult-types-file hyperreal.so.types -o $@ $<


