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

SHELL := /bin/bash

VERBOSE ?= 0
ifeq ($(VERBOSE),0)
    override _Q = @
    override _CCVMODE =
else ifeq ($(VERBOSE),1)
    override _Q =
    override _CCVMODE =
else ifeq ($(VERBOSE),2)
    override _Q =
    override _CCVMODE = -v
else
    $(error Invalid value ($(VERBOSE)) for the variable "VERBOSE"!)
endif

override _SRCDIR = src

override _SOURCES = $(shell find $(_SRCDIR) -type f -name "*.c" | sed -e 's/^$(_SRCDIR)\///g')

override _MAPDIR = .

override _MAPS = $(shell find $(_MAPDIR) -type f -name "*.map" | sed -e 's/^$(_MAPDIR)\///g')

override _OBJDIR = obj

override _DEPDIR = .depend

override _LIBDIR = lib

override _LIBSONAME = ver_ex

override _INCDIRS = $(_SRCDIR) include

CPPFLAGS += $(_INCDIRS:%=-I%)

CFLAGS += -std=c17 -fPIC

LDFLAGS += $(_MAPS:%=-Wl,--version-script=$(_MAPDIR)/%)

CC := gcc


.DEFAUFT_GOAL := all

.PHONY: all
all: $(_LIBDIR)/lib$(_LIBSONAME).so

$(_LIBDIR)/lib$(_LIBSONAME).so: $(_SOURCES:%.c=$(_OBJDIR)/%.o) $(_MAPS:%=$(_MAPDIR)/%)
	$(_Q)if [ ! -d "$(_LIBDIR)" ]; then mkdir -p "$(_LIBDIR)"; fi
	$(_Q)$(CC) $(_CCVMODE) -shared $(LDFLAGS) -o $@ $(_SOURCES:%.c=$(_OBJDIR)/%.o)

$(_OBJDIR)/%.o: $(_SRCDIR)/%.c
	$(_Q)rm -f $@;
	$(_Q)if [ ! -d "$(@D)" ]; then mkdir -p "$(@D)"; fi
	$(_Q)$(CC) $(_CCVMODE) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(_DEPDIR)/%.d: $(_SRCDIR)/%.c
	$(_Q)rm -f $@
	$(_Q)if [ ! -d "$(@D)" ]; then mkdir -p "$(@D)"; fi
	$(_Q)$(CC) $(_CCVMODE) $(CPPFLAGS) $(CFLAGS) -MM $< > $@.$$$$; \
		sed 's,\($(@:$(_DEPDIR)/%.d=%.o)\)[ :]*,$(_OBJDIR)/\1 $@: ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

NOINCDEPTARGETS += clean
ifeq ($(MAKECMDGOALS),)
    override _INCDEPREQUIRED = yes
else
    override _GOALSWONOINCDEPTARGETS = $(strip $(filter-out $(NOINCDEPTARGETS),$(MAKECMDGOALS)))
    ifneq ($(_GOALSWONOINCDEPTARGETS),)
        override _INCDEPREQUIRED = yes
    endif
endif
ifeq ($(_INCDEPREQUIRED),yes)
    sinclude $(_SOURCES:%.c=$(_DEPDIR)/%.d)
endif

.PHONY: clean
clean:
	$(_Q)rm -rf $(_LIBDIR)
	$(_Q)rm -rf $(_OBJDIR)
	$(_Q)rm -rf $(_DEPDIR)
