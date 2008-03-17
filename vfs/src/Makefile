CC=gcc
AR=ar
CFLAGS=-Wall -g -I../sqlite3.5
OBJS=little.o little-common.o little_locks.o little-ro.o

liblittlevfs.a: $(OBJS)
	$(AR) cr $@ $(OBJS)

clean:
	rm $(OBJS)
