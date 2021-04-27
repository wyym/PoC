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
%%%   This module is the profiling object for profiling ETS table (ets) in
%%%   duplicate bag mode.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ets_duplicate_bag_PROFOBJ).

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

?REG_PROFOBJ("ets-duplicated_bag").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key, Value, Match Pattern and Match Specification Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(KEY(S), {S, S}).
-define(VAL(S), {S, S, S}).

-define(MP_FULL_KEY(S), {?KEY(S), '_'}).
-define(MP_PARTIAL_KEY(S), {{S, '_'}, '_'}).
-define(MP_NO_KEY(S), {'_', ?VAL(S)}).

-define(MS_FULL_KEY(S), [{{?KEY(S), '_'}, [], ['$_']}]).
-define(MS_PARTIAL_KEY(S), [{{{S, '_'}, '_'}, [], ['$_']}]).
-define(MS_NO_KEY(S), [{{'_', {S, '_', '_'}}, [], ['$_']}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Behavioral Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare() -> [{seed, prof_object:seed_driven_profile()}].
prepare() ->
	?ETS_PROFILES.

-spec run(ProfilingSeedSet::prof_object:profiling_seed_set()) -> any().
run({BgSeeds, CSeeds, RSeeds, _USeeds, DSeeds}) ->
	BgData = generate_background_data(BgSeeds),
	Tid1 = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Tid1),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C ets:insert/2", fun test_op_insert/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "R ets:info/2 (size)", fun test_op_info_size/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:lookup/2", fun test_op_lookup/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:lookup_element/3", fun test_op_lookup_element/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:match/2 (full key)", fun test_op_match_full_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:match/2 (partial key)", fun test_op_match_partial_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:match/2 (no key)", fun test_op_match_no_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:match_object/2 (full key)", fun test_op_match_object_full_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:match_object/2 (partial key)", fun test_op_match_object_partial_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:match_object/2 (no key)", fun test_op_match_object_no_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:select/2 (full key)", fun test_op_select_full_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:select/2 (partial key)", fun test_op_select_partial_key/2, [Tid1]),
	prof_object:run_cpu_profiling(RSeeds, "| ets:select/2 (no key)", fun test_op_select_no_key/2, [Tid1]),
	prof_object:run_cpu_profiling(DSeeds, "D ets:delete/2", fun test_op_delete/2, [Tid1]),
	cleanup(Tid1),
	Tid2 = set_background_data(BgData),
	prof_object:run_cpu_profiling(DSeeds, "| ets:delete_object/2", fun test_op_delete_object/2, [Tid2]),
	cleanup(Tid2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_memory(Tid) ->
	Seed = 0,
	Elem = {?KEY(Seed), ?VAL(Seed)},
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = ets:info(Tid, memory) * WordSize,
	{Size, FlatSize, TotalSize, TotalSize}.
test_zero_load(_Seed, _Args) ->
	ok.

test_op_insert(Seed, [Tid]) ->
	ets:insert(Tid, {?KEY(Seed), ?VAL(Seed)}).

test_op_info_size(_Seed, [Tid]) ->
	ets:info(Tid, size).

test_op_lookup(Seed, [Tid]) ->
	ets:lookup(Tid, ?KEY(Seed)).

test_op_lookup_element(Seed, [Tid]) ->
	ets:lookup_element(Tid, ?KEY(Seed), 1).

test_op_match_full_key(Seed, [Tid]) ->
	ets:match(Tid, ?MP_FULL_KEY(Seed)).

test_op_match_partial_key(Seed, [Tid]) ->
	ets:match(Tid, ?MP_PARTIAL_KEY(Seed)).

test_op_match_no_key(Seed, [Tid]) ->
	ets:match(Tid, ?MP_NO_KEY(Seed)).

test_op_match_object_full_key(Seed, [Tid]) ->
	ets:match_object(Tid, ?MP_FULL_KEY(Seed)).

test_op_match_object_partial_key(Seed, [Tid]) ->
	ets:match_object(Tid, ?MP_PARTIAL_KEY(Seed)).

test_op_match_object_no_key(Seed, [Tid]) ->
	ets:match_object(Tid, ?MP_NO_KEY(Seed)).

test_op_select_full_key(Seed, [Tid]) ->
	ets:select(Tid, ?MS_FULL_KEY(Seed)).

test_op_select_partial_key(Seed, [Tid]) ->
	ets:select(Tid, ?MS_PARTIAL_KEY(Seed)).

test_op_select_no_key(Seed, [Tid]) ->
	ets:select(Tid, ?MS_NO_KEY(Seed)).

test_op_delete(Seed, [Tid]) ->
	ets:delete(Tid, ?KEY(Seed)).

test_op_delete_object(Seed, [Tid]) ->
	ets:delete_object(Tid, ?KEY(Seed)).

set_background_data(Data) ->
	Tid = ets:new(test_db, [duplicate_bag, public, {keypos, 1}]),
	ets:insert(Tid, Data),
	Tid.

cleanup(Tid) ->
	ets:delete(Tid).

generate_background_data(Seeds) ->
	[{?KEY(S), ?VAL(S)} || S <- Seeds].
