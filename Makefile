.PHONY: test
test:
	timeout 3s jbuilder runtest

.PHONY: clean
clean:
	rm -rf _build
