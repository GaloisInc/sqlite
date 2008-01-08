GHC=ghc
ODIR=.ghc

all: $(ODIR)
	sqlite3 test.db < test.sql
	$(GHC) -odir=$(ODIR) -hidir=$(ODIR) --make Test.hs -o test
	gdb ./test

$(ODIR):
	mkdir -p $(ODIR)

clean:
	$(RM) -r $(ODIR)
	$(RM) test
	$(RM) test.db

.PHONY: all clean
