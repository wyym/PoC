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
##   This makefile is used to compile the Erlang source code files under the
##   "src" folder into beam files.
## 
##   When doing compilation, parallel compilation is supported with additional
##   option "-j".
## 
## Exported Targets:
## 
##   It supports to make below targets:
## 
##   - build
## 
##     Compile all the Erlang source code into beam files. It is the default
##     target if no target specified.
## 
##   - clean
## 
##     Remove all the generated objects and files during compilation.
## 
## Exported Arguments:
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
## 
## Compilation Configuration File:
## 
##   If extra compilation and building options are required, a "mk.conf" file
##   could be created under the component folder. Below variables are supported
##   to be defined in this file:
## 
##   - LOCAL_LIBS
## 
##     This variable is used to define the directories of dependent components.
## 
##   - LOCAL_CFLAGS
## 
##     This variable is used to define the extra compilation options for Erlang
##     source files.
## 
##   - LOCAL_DBGFLAGS
## 
##     This variable is used to define the extra debug options for Erlang
##     source files. It will be used when "DEBUG" is set to "on".
###############################################################################

## ----- Constants and Environment -----
SHELL := /bin/bash

RM := rm -f

ERLC := erlc

SRCSUBDIR = src

DEPSUBDIR = .depend

EBINSUBDIR = ebin

INCSUBDIR = include

TARGETDIR = .

TARGETDEPDIR = $(shell echo $(TARGETDIR)/$(DEPSUBDIR) | sed -e 's/^\.\///')
TARGETEBINDIR = $(shell echo $(TARGETDIR)/$(EBINSUBDIR) | sed -e 's/^\.\///')

sinclude mk.conf

CFLAGS = $(LOCAL_CFLAGS)

LIBDIRS = $(LOCAL_LIBS:%=%/$(EBINSUBDIR))

INCDIRS = $(if $(shell [ -d "$(INCSUBDIR)" ] && echo "yes"),$(INCSUBDIR)) \
		  $(foreach d,$(LOCAL_LIBS:%=%/$(INCSUBDIR)),$(if $(shell [ -d "$d" ] && echo "yes"),$d))

BEAMS = $(shell find $(SRCSUBDIR) -type f -name "*.erl" -print | sed -e 's/^\.\///' -e 's/^$(SRCSUBDIR)\///' -e 's/\.erl$$/\.beam/')


## ----- Argument Detection -----
ifeq ($(DEBUG), on)
DBGFLAGS = -DDEBUG $(LOCAL_DBGFLAGS)
else
DBGFLAGS = 
endif

ifeq ($(VERBOSE), on)
VMODE = -v
else
VMODE =
endif


## ----- Target Definition -----
.PHONY: build clean

build:: $(BEAMS:%=$(TARGETEBINDIR)/%)

clean::
	$(RM) -r $(TARGETEBINDIR)
	$(RM) -r $(TARGETDEPDIR)

$(TARGETDEPDIR)/%.d: $(SRCSUBDIR)/%.erl
	@ if [ ! -d $(@D) ]; then \
		mkdir -p $(@D); \
	fi
	$(ERLC) $(VMODE) $(CFLAGS) $(DBGFLAGS) $(INCDIRS:%=-I%) $(LIBDIRS:%=-pz %) -M -MG -MT $(@:$(TARGETDEPDIR)/%.d=$(TARGETEBINDIR)/%.beam) -MF $@ -- $<

$(TARGETEBINDIR)/%.beam: $(SRCSUBDIR)/%.erl
	@ if [ ! -d $(@D) ]; then \
		mkdir -p $(@D); \
	fi
	$(ERLC) $(VMODE) $(CFLAGS) $(DBGFLAGS) $(INCDIRS:%=-I%) $(LIBDIRS:%=-pz %) -o $(@D) -- $<

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), test)
sinclude $(BEAMS:%.beam=$(TARGETDEPDIR)/%.d)
endif
endif
