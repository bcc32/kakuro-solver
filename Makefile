all: kakuro

kakuro:
	mkdir -p bin/
	sbcl --noinform --load src/kakuro.lisp --eval "(sb-ext:save-lisp-and-die \"bin/kakuro\" :executable t :toplevel #'kakuro:main)"

clean:
	rm -rf bin/
