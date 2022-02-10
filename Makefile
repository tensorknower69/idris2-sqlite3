LIBIDRIS2_CUTILS := libidris2_cutils.so

all: $(LIBIDRIS2_CUTILS)

$(LIBIDRIS2_CUTILS): src_c/idris2_cutils.c
	$(CC) -shared -o $@ $^

.PHONY: clean
clean:
	rm -f $(LIBIDRIS2_CUTILS)
