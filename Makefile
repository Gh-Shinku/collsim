DUNE ?= dune

.PHONY: all build run test fmt fmt-check clean

all: build

build:
	@$(DUNE) build

run:
	@$(DUNE) exec ./bin/main.exe -- $(ARGS)

test:
	@$(DUNE) runtest

fmt:
	@$(DUNE) fmt

fmt-check:
	@$(DUNE) build @fmt

clean:
	@$(DUNE) clean
