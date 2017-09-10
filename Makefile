.PHONY: test
test:
	README=$(PWD)/README.md timeout 3s jbuilder runtest

.PHONY: clean
clean:
	rm -rf _build
