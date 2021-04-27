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
%%%   This module is the profiling object for profiling General Balanced Trees
%%%   (gb_trees) data structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gb_trees_PROFOBJ).

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

?REG_PROFOBJ("gb_trees").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key and Value Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(KEY(S), {S, S}).
-define(VAL(S), {S, S, S}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Behavioral Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare() -> [{seed, prof_object:seed_driven_profile()}].
prepare() ->
	?NONE_ETS_PROFILES.

-spec run(ProfilingSeedSet::prof_object:profiling_seed_set()) -> any().
run({BgSeeds, CSeeds, RSeeds, USeeds, DSeeds}) ->
	BgData = generate_background_data(BgSeeds),
	Tree = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Tree),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C gb_trees:from_orddict/1", fun test_op_from_orddict/2, [orddict:from_list(BgData)]),
	prof_object:run_cpu_profiling(CSeeds, "| gb_trees:insert/3", fun test_op_insert/2, [Tree]),
	prof_object:run_cpu_profiling(CSeeds, "| gb_trees:enter/3 (no exist)", fun test_op_enter/2, [Tree]),
	prof_object:run_cpu_profiling(RSeeds, "R gb_trees:size/1", fun test_op_size/2, [Tree]),
	prof_object:run_cpu_profiling(RSeeds, "| gb_trees:lookup/2", fun test_op_lookup/2, [Tree]),
	prof_object:run_cpu_profiling(RSeeds, "| gb_trees:get/2", fun test_op_get/2, [Tree]),
	prof_object:run_cpu_profiling(RSeeds, "| gb_trees:to_list/1", fun test_op_to_list/2, [Tree]),
	prof_object:run_cpu_profiling(USeeds, "U gb_trees:enter/3 (exist)", fun test_op_enter/2, [Tree]),
	prof_object:run_cpu_profiling(USeeds, "| gb_trees:update/3", fun test_op_update/2, [Tree]),
	prof_object:run_cpu_profiling(DSeeds, "D gb_trees:delete/2", fun test_op_delete/2, [Tree]),
	prof_object:run_cpu_profiling(DSeeds, "| gb_trees:delete_any/2", fun test_op_delete_any/2, [Tree]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Tree) ->
	Seed = 0,
	Elem = {?KEY(Seed), ?VAL(Seed)},
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Tree) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Tree) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_from_orddict(_Seed, [Data]) ->
	gb_trees:from_orddict(Data).

test_op_insert(Seed, [Tree]) ->
	gb_trees:insert(?KEY(Seed), ?VAL(Seed), Tree).

test_op_size(_Seed, [Tree]) ->
	gb_trees:size(Tree).

test_op_lookup(Seed, [Tree]) ->
	gb_trees:lookup(?KEY(Seed), Tree).

test_op_get(Seed, [Tree]) ->
	gb_trees:get(?KEY(Seed), Tree).

test_op_to_list(_Seed, [Tree]) ->
	gb_trees:to_list(Tree).

test_op_update(Seed, [Tree]) ->
	gb_trees:update(?KEY(Seed), ?VAL(Seed), Tree).

test_op_enter(Seed, [Tree]) ->
	gb_trees:enter(?KEY(Seed), ?VAL(Seed), Tree).

test_op_delete(Seed, [Tree]) ->
	gb_trees:delete(?KEY(Seed), Tree).

test_op_delete_any(Seed, [Tree]) ->
	gb_trees:delete_any(?KEY(Seed), Tree).

set_background_data(Data) ->
	OrderedData = orddict:from_list(Data),
	gb_trees:from_orddict(OrderedData).

generate_background_data(Seeds) ->
	[{?KEY(S), ?VAL(S)} || S <- Seeds].
