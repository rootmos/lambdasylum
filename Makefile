.PHONY: go
go: build
	timeout 3s _build/default/silly_f.exe

.PHONY: build
build:
	jbuilder build silly_f.exe
