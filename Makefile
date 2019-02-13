.PHONY: nix

all:
	cd include && make all
	cd cbits && make all

nix:
	cd include && make nix
	cd cbits && make nix

clean:
	cd include && make clean
	cd cbits && make clean