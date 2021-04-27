###############################################################################
## Copyright (c) 2021 Ji Zhu
## 
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
## 
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
## 
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
## 
## 
## Description:
## 
##   This makefile is used to compile all the Erlang source code into beam
##   files. It also provide the interface to launch profiling.
## 
##   When doing compilation, parallel compilation is supported with additional
##   option "-j".
## 
## Exported Targets:
## 
##   It supports to make below targets:
## 
##   - all
## 
##     Same as "make configure" and then "make build". It is the default target
##     if no target specified.
## 
##   - configure
## 
##     Setup the compilation and build environment.
## 
##   - build
## 
##     Compile all the Erlang source code into beam files.
## 
##   - clean
## 
##     Remove all the generated objects and files during compilation.
## 
##   - profile
## 
##     Launch the performance testing and profiling.
## 
## Exported Arguments:
## 
##   - PROFOBJS
## 
##     This argument is used to indicate the Profiling Object(s) to be involved
##     for performance profiling. The value should be the name of the Profiling
##     Object. If multiple Profiling Objects to be involved, separate their
##     names with space. If this argument is not specified, all the Profiling
##     Objects will be involved. It is only effective when making the target
##     "profile". To use it like below:
## 
##       make profile PROFOBJS="dict sets"
## 
##   - VERBOSE
## 
##     This argument is used to turn on or off the verbose mode. When it is
##     specified as "on", which means enabling verbose mode, then there will be
##     lots of detailed information generated during building. Any other value
##     or if it is not specified, it means disabling verbose mode. To use it
##     like below:
## 
##       make VERBOSE=on
## 
##   - DEBUG
## 
##     This argument is used to turn on or off the debug mode. When it is
##     specified as "on", which means enabling debug mode, then the source
##     code with debug information will be involved. Actually, in this case,
##     "-DDEBUG" will be appended to the preprocessing options and passed to
##     the compiler. Any other value or if it is not specified, it means
##     disabling debug mode. To use it like below:
## 
##       make DEBUG=on
###############################################################################

## ----- Constants and Environment -----
SHELL := /bin/bash

RM := rm -f

PRJROOT := $(shell pwd)

PLATFORMSUBDIRS = fw \
				  app

PLATFORMDIRS = $(PLATFORMSUBDIRS:%=$(PRJROOT)/%)

PROFOBJROOTDIR = $(PRJROOT)/objects

PROFOBJDIRS = $(shell find $(PROFOBJROOTDIR) -mindepth 1 -maxdepth 1 -type d)

COMPTMKFILE = component.mk

EBINSUBDIR = ebin

TAREBINDIRS = $(PLATFORMDIRS:%=%/$(EBINSUBDIR)) $(PROFOBJDIRS:%=%/$(EBINSUBDIR))


## ----- Target Definition -----
.NOTPARALLEL: all build

.PHONY: all \
		configure \
		build build_platform build_profobjs \
		$(PLATFORMDIRS) $(PROFOBJDIRS) \
		clean \
		profile

all: configure build

configure::
	@ for dir in $(PLATFORMDIRS) $(PROFOBJDIRS); do \
		echo -e "Configuring $$dir"; \
		echo -e "include `realpath --relative-to=$$dir $(PRJROOT)`/$(COMPTMKFILE)" > $$dir/Makefile; \
	done

build:: build_platform build_profobjs

build_platform: $(PLATFORMDIRS)

build_profobjs: $(PROFOBJDIRS)

$(PLATFORMDIRS) $(PROFOBJDIRS):
	@ exec $(MAKE) -C $@

clean::
	@ for dir in $(PROFOBJDIRS) $(PLATFORMDIRS); do \
		if [ -f "$$dir/Makefile" ]; then \
			$(MAKE) -C $$dir clean; \
			$(RM) $$dir/Makefile; \
		fi \
	done

profile::
	@ exec ./run_prof.sh $(TAREBINDIRS:%=-d %) $(PROFOBJS)
