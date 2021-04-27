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
%%%   This module is the profiling object for profiling ordered Dictionary
%%%   (orddict) data structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(orddict_PROFOBJ).

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

?REG_PROFOBJ("orddict").


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
	Dict = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, Dict),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C orddict:from_list/1", fun test_op_from_list/2, [BgData]),
	prof_object:run_cpu_profiling(CSeeds, "| orddict:append/3", fun test_op_append/2, [Dict]),
	prof_object:run_cpu_profiling(CSeeds, "| orddict:store/3 (no exist)", fun test_op_store/2, [Dict]),
	prof_object:run_cpu_profiling(RSeeds, "R orddict:size/1", fun test_op_size/2, [Dict]),
	prof_object:run_cpu_profiling(RSeeds, "| orddict:find/2", fun test_op_find/2, [Dict]),
	prof_object:run_cpu_profiling(RSeeds, "| orddict:fetch/2", fun test_op_fetch/2, [Dict]),
	prof_object:run_cpu_profiling(RSeeds, "| orddict:to_list/1", fun test_op_to_list/2, [Dict]),
	prof_object:run_cpu_profiling(USeeds, "U orddict:store/3 (exist)", fun test_op_store/2, [Dict]),
	prof_object:run_cpu_profiling(USeeds, "| orddict:update/3", fun test_op_update/2, [Dict]),
	prof_object:run_cpu_profiling(DSeeds, "D orddict:erase/2", fun test_op_erase/2, [Dict]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(Dict) ->
	Seed = 0,
	Elem = {?KEY(Seed), ?VAL(Seed)},
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(Dict) * WordSize,
	TotalFlatSize = erts_debug:flat_size(Dict) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_from_list(_Seed, [Data]) ->
	orddict:from_list(Data).

test_op_append(Seed, [Dict]) ->
	orddict:append(?KEY(Seed), ?VAL(Seed), Dict).

test_op_store(Seed, [Dict]) ->
	orddict:store(?KEY(Seed), ?VAL(Seed), Dict).

test_op_size(_Seed, [Dict]) ->
	orddict:size(Dict).

test_op_find(Seed, [Dict]) ->
	orddict:find(?KEY(Seed), Dict).

test_op_fetch(Seed, [Dict]) ->
	orddict:fetch(?KEY(Seed), Dict).

test_op_to_list(_Seed, [Dict]) ->
	orddict:to_list(Dict).

test_op_update(Seed, [Dict]) ->
	orddict:update(?KEY(Seed), fun(_V) -> ?VAL(Seed) end, Dict).

test_op_erase(Seed, [Dict]) ->
	orddict:erase(?KEY(Seed), Dict).

set_background_data(Data) ->
	orddict:from_list(Data).

generate_background_data(Seeds) ->
	[{?KEY(S), ?VAL(S)} || S <- Seeds].
