.PHONY: test
test:
	README=$(PWD)/README.md timeout 3s jbuilder runtest

.PHONY: repl
repl:
	jbuilder build repl.exe
	./lambda-rlwrap.sh _build/default/repl.exe

.PHONY: clean
clean:
	rm -rf _build
