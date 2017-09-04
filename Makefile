.PHONY: go
go: build
	_build/default/silly_f.exe

.PHONY: build
build:
	jbuilder build silly_f.exe
