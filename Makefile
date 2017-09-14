.PHONY: test
test:
	README=$(PWD)/README.md timeout 3s jbuilder runtest

.PHONY: repl
repl: repl-build
	./lambda-rlwrap.sh _build/default/repl.exe

.PHONY: repl-build
repl-build:
	jbuilder build repl.exe

.PHONY: clean
clean:
	rm -rf _build
