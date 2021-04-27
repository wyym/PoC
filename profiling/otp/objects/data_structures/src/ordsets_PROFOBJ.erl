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
%%%   This module is the profiling object for profiling ordered Set (ordsets)
%%%   data structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ordsets_PROFOBJ).

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

?REG_PROFOBJ("ordsets").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key and Value Definition
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
	Set = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Set),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C ordsets:from_list/1", fun test_op_from_list/2, [BgData]),
	prof_object:run_cpu_profiling(CSeeds, "| ordsets:add_element/2", fun test_op_add_element/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "R ordsets:size/1", fun test_op_size/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "| ordsets:is_element/2", fun test_op_is_element/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "| ordsets:to_list/1", fun test_op_to_list/2, [Set]),
	prof_object:run_cpu_profiling(DSeeds, "D ordsets:del_element/2", fun test_op_del_element/2, [Set]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Set) ->
	Seed = 0,
	Elem = ?ELEM(Seed),
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Set) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Set) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_from_list(_Seed, [Data]) ->
	ordsets:from_list(Data).

test_op_add_element(Seed, [Set]) ->
	ordsets:add_element(?ELEM(Seed), Set).

test_op_size(_Seed, [Set]) ->
	ordsets:size(Set).

test_op_is_element(Seed, [Set]) ->
	ordsets:is_element(?ELEM(Seed), Set).

test_op_to_list(_Seed, [Set]) ->
	ordsets:to_list(Set).

test_op_del_element(Seed, [Set]) ->
	ordsets:del_element(?ELEM(Seed), Set).

set_background_data(Data) ->
	ordsets:from_list(Data).

generate_background_data(Seeds) ->
	[?ELEM(S) || S <- Seeds].
