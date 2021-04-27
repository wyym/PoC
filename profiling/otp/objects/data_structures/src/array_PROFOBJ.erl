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
%%%   This module is the profiling object for profiling array data structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(array_PROFOBJ).

-namespace('eppp.objects.data_structures').

-visibility(public).

-vsn(20191012).


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

?REG_PROFOBJ("array").


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
run({BgSeeds, CSeeds, RSeeds, USeeds, DSeeds}) ->
	BgCapacity = erlang:length(BgSeeds),
	RIndexedData = generate_indexed_data(RSeeds, BgCapacity),
	UIndexedData = generate_indexed_data(USeeds, BgCapacity),
	DIndexedData = generate_indexed_data(DSeeds, BgCapacity),
	IndexedBgData = generate_indexed_background_data(BgSeeds),
	BgData = generate_background_data(BgSeeds),
	Array = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Array),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C array:from_list/1", fun test_op_from_list/2, [BgData]),
	prof_object:run_cpu_profiling(CSeeds, "| array:from_orddict/1", fun test_op_from_orddict/2, [IndexedBgData]),
	prof_object:run_cpu_profiling(RSeeds, "R array:size/1", fun test_op_size/2, [Array]),
	prof_object:run_cpu_profiling(RIndexedData, "| array:get/2", fun test_op_get/2, [Array]),
	prof_object:run_cpu_profiling(RSeeds, "| array:to_list/1", fun test_op_to_list/2, [Array]),
	prof_object:run_cpu_profiling(RSeeds, "| array:to_orddict/1", fun test_op_to_orddict/2, [Array]),
	prof_object:run_cpu_profiling(RSeeds, "| array:sparse_to_list/1", fun test_op_sparse_to_list/2, [Array]),
	prof_object:run_cpu_profiling(RSeeds, "| array:sparse_to_orddict/1", fun test_op_sparse_to_orddict/2, [Array]),
	prof_object:run_cpu_profiling(UIndexedData, "U array:set/3", fun test_op_set/2, [Array]),
	prof_object:run_cpu_profiling(DIndexedData, "D array:reset/2", fun test_op_reset/2, [Array]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Array) ->
	Seed = 0,
	Elem = ?ELEM(Seed),
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Array) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Array) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_from_list(_Seed, [Data]) ->
	array:from_list(Data).

test_op_from_orddict(_Seed, [Data]) ->
	array:from_orddict(Data).

test_op_size(_Seed, [Array]) ->
	array:size(Array).

test_op_get({Index, _Seed}, [Array]) ->
	array:get(Index, Array).

test_op_to_list(_Seed, [Array]) ->
	array:to_list(Array).

test_op_to_orddict(_Seed, [Array]) ->
	array:to_orddict(Array).

test_op_sparse_to_list(_Seed, [Array]) ->
	array:sparse_to_list(Array).

test_op_sparse_to_orddict(_Seed, [Array]) ->
	array:sparse_to_orddict(Array).

test_op_set({Index, Seed}, [Array]) ->
	array:set(Index, ?ELEM(Seed), Array).

test_op_reset({Index, _Seed}, [Array]) ->
	array:reset(Index, Array).

set_background_data(Data) ->
	array:from_list(Data).

generate_background_data(Seeds) ->
	[?ELEM(S) || S <- Seeds].

generate_indexed_background_data(Seeds) ->
	MaxIndex = erlang:length(Seeds) - 1,
	generate_indexed_background_data_help(Seeds, MaxIndex, []).

generate_indexed_background_data_help([Seed | RestSeeds], Index, Data) when Index >= 0 ->
	generate_indexed_background_data_help(RestSeeds, Index - 1, [{Index, Seed}| Data]);

generate_indexed_background_data_help([], _Index, Data) ->
	Data.

generate_indexed_data(Seeds, Length) ->
	generate_indexed_data_help(Seeds, Length, []).

generate_indexed_data_help([Seed | RestSeeds], Length, Data) ->
	Index = rand:uniform(Length),
	generate_indexed_data_help(RestSeeds, Length, [{Index, ?ELEM(Seed)} | Data]);

generate_indexed_data_help([], _Length, Data) ->
	Data.
