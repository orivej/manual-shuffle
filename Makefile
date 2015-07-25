LISP = sbcl
SOURCES = $(shell find '.' -name '*.asd' -o -name '*.lisp')

bin/manual-shuffle: $(SOURCES)
	$(LISP) --eval '(asdf:load-system :manual-shuffle.gui)' --eval '(qtools::build-qt-system :manual-shuffle.gui :force t)'
