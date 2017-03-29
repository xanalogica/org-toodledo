# tools to use
CASK ?= cask

all: test

test:
	${CASK} exec buttercup -L . -L tests

.PHONY: all test

# Development dependencies are installed with 'cask install', but are not
# included in package descriptors generated by 'cask pkg-file' and 'cask
# package'.

# Notes re Reduction in Use of Makefile targets and 'Keeping It Simple':
#   Autoloads are auto-generated at package installation time, not here.
#   Dependency packages are installed using 'cask install', not here.
#   Compiled bytecode is generated by 'cask build', not here.
#   Distribution tarballs are built by 'cask package', not here.
#   Package metadata (i.e. PACKAGE-pkg.el) is generated by 'cask pkg-file'.
#   To validate .travis.yml, just use 'travis lint'.
#   To run coverage report locally, invoke 'make test' with TRAVIS=true
#   (to push results to coveralls.io, create a .coveralls file with your repo token)
