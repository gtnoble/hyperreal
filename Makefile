CSC = csc
CSC_FLAGS = -O3 -debug o -verbose
CSC_SO_FLAGS = $(CSC_FLAGS) -s
CSC_BIN_FLAGS = $(CSC_FLAGS)

.PHONY:all

all: hyperreal.test two-port-network.test

.PHONY:test

test: hyperreal.test two-port-network.test
	./hyperreal.test
	./two-port-network.test

.PHONY:libs

libs: hyperreal.test.so hyperreal.so

.PHONY:clean

clean:
	rm -f *.so *.test PROFILE.* *.types

hyperreal.so hyperreal.so.types: hyperreal.scm types.scm
	$(CSC) $(CSC_SO_FLAGS) -prologue types.scm -emit-types-file $(basename $<).so.types $<

%.so %.so.types: %.scm types.scm hyperreal.so hyperreal.so.types
	$(CSC) $(CSC_SO_FLAGS) -prologue types.scm -consult-types-file hyperreal.so.types -emit-types-file $(basename $<).so.types $<

%.test: %.test.scm %.so %.so.types
	$(CSC) $(CSC_BIN_FLAGS) -consult-types-file $(basename $@).so.types -o $@ $<
