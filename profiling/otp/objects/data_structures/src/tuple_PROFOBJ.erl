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
%%%   This module is the profiling object for profiling tuple data structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(tuple_PROFOBJ).

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

?REG_PROFOBJ("tuple").


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
	CIndexedData = generate_indexed_data(CSeeds, BgCapacity),
	RIndexedData = generate_indexed_data(RSeeds, BgCapacity),
	UIndexedData = generate_indexed_data(USeeds, BgCapacity),
	DIndexedData = generate_indexed_data(DSeeds, BgCapacity),
	BgData = generate_background_data(BgSeeds),
	Tuple = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Tuple),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C erlang:list_to_tuple/1", fun test_op_list_to_tuple/2, [BgData]),
	prof_object:run_cpu_profiling(CSeeds, "| erlang:append_element/2", fun test_op_append_element/2, [Tuple]),
	prof_object:run_cpu_profiling(CIndexedData, "| erlang:insert_element/3", fun test_op_insert_element/2, [Tuple]),
	prof_object:run_cpu_profiling(RSeeds, "R erlang:tuple_size/1", fun test_op_tuple_size/2, [Tuple]),
	prof_object:run_cpu_profiling(RSeeds, "| erlang:size/1", fun test_op_size/2, [Tuple]),
	prof_object:run_cpu_profiling(RIndexedData, "| erlang:element/2", fun test_op_element/2, [Tuple]),
	prof_object:run_cpu_profiling(RSeeds, "| erlang:tuple_to_list/1", fun test_op_tuple_to_list/2, [Tuple]),
	prof_object:run_cpu_profiling(UIndexedData, "U erlang:setelement/3", fun test_op_setelement/2, [Tuple]),
	prof_object:run_cpu_profiling(DIndexedData, "D erlang:delete_element/2", fun test_op_delete_element/2, [Tuple]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Tuple) ->
	Seed = 0,
	Elem = ?ELEM(Seed),
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Tuple) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Tuple) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_list_to_tuple(_Seed, [Data]) ->
	erlang:list_to_tuple(Data).

test_op_append_element(Seed, [Tuple]) ->
	erlang:append_element(Tuple, ?ELEM(Seed)).

test_op_insert_element({Index, Seed}, [Tuple]) ->
	erlang:insert_element(Index, Tuple, ?ELEM(Seed)).

test_op_tuple_size(_Seed, [Tuple]) ->
	erlang:tuple_size(Tuple).

test_op_size(_Seed, [Tuple]) ->
	erlang:size(Tuple).

test_op_element({Index, _Seed}, [Tuple]) ->
	erlang:element(Index, Tuple).

test_op_tuple_to_list(_Seed, [Tuple]) ->
	erlang:tuple_to_list(Tuple).

test_op_setelement({Index, Seed}, [Tuple]) ->
	erlang:setelement(Index, Tuple, ?ELEM(Seed)).

test_op_delete_element({Index, _Seed}, [Tuple]) ->
	erlang:delete_element(Index, Tuple).

set_background_data(Data) ->
	erlang:list_to_tuple(Data).

generate_background_data(Seeds) ->
	[?ELEM(S) || S <- Seeds].

generate_indexed_data(Seeds, Length) ->
	generate_indexed_data_help(Seeds, Length, []).

generate_indexed_data_help([Seed | RestSeeds], Length, Data) ->
	Index = rand:uniform(Length),
	generate_indexed_data_help(RestSeeds, Length, [{Index, ?ELEM(Seed)} | Data]);

generate_indexed_data_help([], _Length, Data) ->
	Data.
