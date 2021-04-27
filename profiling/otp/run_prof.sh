#!/bin/bash
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
##   This script is used to launch an Erlang shell to run the performance
##   profiling. The process will be bound to CPU core 0 and Erlang/OTP SMP
##   feature is disabled.
###############################################################################

## ----- Constants -----
PROGNAME=$(basename $0)

ERL="erl"

## ----- Function Definition -----
function help(){
	cat <<EOF
Usage:
  $PROGNAME [-d <Beam Dir>] [<Prof Obj Name>]

Options:
  -d, --directory
    Indicate the directory with Erlang beam file(s) to be involved. Multiple
    directories could be specified if necessary.

  -h, --help
    Show this information.

Arguments:
  <Prof Obj Name>
    Indicate the names of the profiling objects to be involved for performance
    profiling. Multiple profiling object names could be specified. If no name
    specified, then all the detected profiling objects would be involved for
    performance profiling.
EOF
	exit 0
}

function error(){
	echo -e "$1" >/dev/stderr
	exit 1
}

function argument_error(){
	error "ERROR: invalid arguments"
}

## ----- Main -----
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
	if (( $# > 1 )); then
		argument_error
	fi
	help
fi

typeset str
typeset paths
typeset beampaths
typeset profobjnames

while (( $# > 0 )); do
	if [[ $1 == "-d" || $1 == "--directory" ]]; then
		shift 1
		paths="$paths $1"
		beampaths="$beampaths,\"$1\""
	elif [[ $1 =~ ^--directory=.* ]]; then
		str="${1#--directory=}"
		paths="$paths $str"
		beampaths="$beampaths,\"$str\""
	else
		profobjnames="$profobjnames,\"$1\""
	fi
	shift 1
done

beampaths=${beampaths#,}
profobjnames=${profobjnames#,}

taskset -pc 0 $$ >/dev/null

exec $ERL -noshell -smp disable -pa $paths -eval "prof_repo:run([$beampaths], [$profobjnames])" -s erlang halt
