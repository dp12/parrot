EMACS ?= emacs
CASK ?= cask

all: test

test: ecukes

ecukes:
	${CASK} exec ecukes

install:
	${CASK} install

.PHONY: all test ecukes install
