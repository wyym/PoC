%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2021 Ji Zhu
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%% 
%%% 
%%% Description:
%%% 
%%%   This module is the profiling object for profiling Queue (queue) data
%%%   structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(queue_PROFOBJ).

-namespace('eppp.objects.data_structures').

-visibility(public).

-vsn(20190628).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behavior Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-behavior(prof_object).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Profiling Object Behavioral Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
	prepare/0,
	run/1
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include Files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("prof_object.hrl").
-include("data_structure_profile_def.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Registration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?REG_PROFOBJ("queue").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Element Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ELEM(S), {{S, S}, {S, S, S}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Behavioral Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare() -> [{seed, prof_object:seed_driven_profile()}].
prepare() ->
	?NONE_ETS_PROFILES.

-spec run(ProfilingSeedSet::prof_object:profiling_seed_set()) -> any().
run({BgSeeds, CSeeds, RSeeds, _USeeds, DSeeds}) ->
	BgData = generate_background_data(BgSeeds),
	Queue = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Queue),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C queue:in/2", fun test_op_in/2, [Queue]),
	prof_object:run_cpu_profiling(CSeeds, "| queue:in_r/2", fun test_op_in_r/2, [Queue]),
	prof_object:run_cpu_profiling(RSeeds, "R queue:len/1", fun test_op_len/2, [Queue]),
	prof_object:run_cpu_profiling(RSeeds, "| queue:member/2", fun test_op_member/2, [Queue]),
	prof_object:run_cpu_profiling(DSeeds, "D queue:out/1", fun test_op_out/2, [Queue]),
	prof_object:run_cpu_profiling(DSeeds, "| queue:out_r/1", fun test_op_out_r/2, [Queue]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Queue) ->
	Seed = 0,
	Elem = ?ELEM(Seed),
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Queue) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Queue) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_in(Seed, [Queue]) ->
	queue:in(?ELEM(Seed), Queue).

test_op_in_r(Seed, [Queue]) ->
	queue:in_r(?ELEM(Seed), Queue).

test_op_len(_Seed, [Queue]) ->
	queue:len(Queue).

test_op_member(Seed, [Queue]) ->
	queue:member(?ELEM(Seed), Queue).

test_op_out(_Seed, [Queue]) ->
	queue:out(Queue).

test_op_out_r(_Seed, [Queue]) ->
	queue:out_r(Queue).

set_background_data(Data) ->
	queue:from_list(Data).

generate_background_data(Seeds) ->
	[?ELEM(S) || S <- Seeds].
