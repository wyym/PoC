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
%%%   This module is the profiling object for profiling Map (maps) data
%%%   structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(maps_PROFOBJ).

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

?REG_PROFOBJ("maps").


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
	Map = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Map),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C maps:from_list/1", fun test_op_from_list/2, [BgData]),
	prof_object:run_cpu_profiling(CSeeds, "| maps:put/3 (no exist)", fun test_op_put/2, [Map]),
	prof_object:run_cpu_profiling(RSeeds, "R maps:size/1", fun test_op_size/2, [Map]),
	prof_object:run_cpu_profiling(RSeeds, "| maps:find/2", fun test_op_find/2, [Map]),
	prof_object:run_cpu_profiling(RSeeds, "| maps:get/2", fun test_op_get/2, [Map]),
	prof_object:run_cpu_profiling(RSeeds, "| maps:to_list/1", fun test_op_to_list/2, [Map]),
	prof_object:run_cpu_profiling(USeeds, "U maps:put/3 (exist)", fun test_op_put/2, [Map]),
	prof_object:run_cpu_profiling(USeeds, "| maps:update/3", fun test_op_update/2, [Map]),
	prof_object:run_cpu_profiling(USeeds, "| maps:update_with/3", fun test_op_update_with/2, [Map]),
	prof_object:run_cpu_profiling(DSeeds, "D maps:remove/2", fun test_op_remove/2, [Map]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Map) ->
	Seed = 0,
	Elem = {?KEY(Seed), ?VAL(Seed)},
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Map) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Map) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_from_list(_Seed, [Data]) ->
	maps:from_list(Data).

test_op_put(Seed, [Map]) ->
	maps:put(?KEY(Seed), ?VAL(Seed), Map).

test_op_size(_Seed, [Map]) ->
	maps:size(Map).

test_op_find(Seed, [Map]) ->
	maps:find(?KEY(Seed), Map).

test_op_get(Seed, [Map]) ->
	maps:get(?KEY(Seed), Map).

test_op_to_list(_Seed, [Map]) ->
	maps:to_list(Map).

test_op_update(Seed, [Map]) ->
	maps:update(?KEY(Seed), ?VAL(Seed), Map).

test_op_update_with(Seed, [Map]) ->
	maps:update_with(?KEY(Seed), fun(_V) -> ?VAL(Seed) end, Map).

test_op_remove(Seed, [Map]) ->
	maps:remove(?KEY(Seed), Map).

set_background_data(Data) ->
	maps:from_list(Data).

generate_background_data(Seeds) ->
	[{?KEY(S), ?VAL(S)} || S <- Seeds].
