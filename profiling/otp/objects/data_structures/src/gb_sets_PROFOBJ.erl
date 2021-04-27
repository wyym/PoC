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
%%%   This module is the profiling object for profiling Set in General Balanced
%%%   Trees implementation (gb_sets) data structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gb_sets_PROFOBJ).

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

?REG_PROFOBJ("gb_sets").


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
	Set = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Set),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C gb_sets:from_list/1", fun test_op_from_list/2, [BgData]),
	prof_object:run_cpu_profiling(CSeeds, "| gb_sets:from_ordset/1", fun test_op_from_ordset/2, [ordsets:from_list(BgData)]),
	prof_object:run_cpu_profiling(CSeeds, "| gb_sets:add_element/2", fun test_op_add_element/2, [Set]),
	prof_object:run_cpu_profiling(CSeeds, "| gb_sets:add/2", fun test_op_add/2, [Set]),
	prof_object:run_cpu_profiling(CSeeds, "| gb_sets:insert/2", fun test_op_insert/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "R gb_sets:size/1", fun test_op_size/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "| gb_sets:is_element/2", fun test_op_is_element/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "| gb_sets:is_member/2", fun test_op_is_member/2, [Set]),
	prof_object:run_cpu_profiling(RSeeds, "| gb_sets:to_list/1", fun test_op_to_list/2, [Set]),
	prof_object:run_cpu_profiling(DSeeds, "D gb_sets:del_element/2", fun test_op_del_element/2, [Set]),
	prof_object:run_cpu_profiling(DSeeds, "| gb_sets:delete/2", fun test_op_delete/2, [Set]),
	prof_object:run_cpu_profiling(DSeeds, "| gb_sets:delete_any/2", fun test_op_delete_any/2, [Set]).


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
	gb_sets:from_list(Data).

test_op_from_ordset(_Seed, [Data]) ->
	gb_sets:from_ordset(Data).

test_op_add_element(Seed, [Set]) ->
	gb_sets:add_element(?ELEM(Seed), Set).

test_op_add(Seed, [Set]) ->
	gb_sets:add(?ELEM(Seed), Set).

test_op_insert(Seed, [Set]) ->
	gb_sets:insert(?ELEM(Seed), Set).

test_op_size(_Seed, [Set]) ->
	gb_sets:size(Set).

test_op_is_element(Seed, [Set]) ->
	gb_sets:is_element(?ELEM(Seed), Set).

test_op_is_member(Seed, [Set]) ->
	gb_sets:is_member(?ELEM(Seed), Set).

test_op_to_list(_Seed, [Set]) ->
	gb_sets:to_list(Set).

test_op_del_element(Seed, [Set]) ->
	gb_sets:del_element(?ELEM(Seed), Set).

test_op_delete(Seed, [Set]) ->
	gb_sets:delete(?ELEM(Seed), Set).

test_op_delete_any(Seed, [Set]) ->
	gb_sets:delete_any(?ELEM(Seed), Set).

set_background_data(Data) ->
	gb_sets:from_list(Data).

generate_background_data(Seeds) ->
	[?ELEM(S) || S <- Seeds].
