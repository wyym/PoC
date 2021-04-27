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
%%%   This module is the profiling object for profiling List (lists) data
%%%   structure for KV storage.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lists_PROFOBJ).

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

?REG_PROFOBJ("lists").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key and Value Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(KEY(S), {S, S}).
-define(VAL(S), {S, S, S}).
-define(ELEM(S), {?KEY(S), ?VAL(S)}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Behavioral Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare() -> [{seed, prof_object:seed_driven_profile()}].
prepare() ->
	?NONE_ETS_PROFILES.

-spec run(ProfilingSeedSet::prof_object:profiling_seed_set()) -> any().
run({BgSeeds, CSeeds, RSeeds, USeeds, DSeeds}) ->
	BgCapacity = erlang:length(BgSeeds),
	RIndexes = generate_indexes(RSeeds, BgCapacity),
	BgData = generate_background_data(BgSeeds),
	List = set_background_data(BgData),
	prof_object:run_mem_profiling(fun test_memory/1, List),
	prof_object:run_cpu_profiling(CSeeds, "No operation (zero load)", fun test_zero_load/2, []),
	prof_object:run_cpu_profiling(CSeeds, "C lists:keystore/4 (no exist)", fun test_op_keystore/2, [List]),
	prof_object:run_cpu_profiling(CSeeds, "| lists:append/1 (head)", fun test_op_append_1_head/2, [List]),
	prof_object:run_cpu_profiling(CSeeds, "| lists:append/1 (tail)", fun test_op_append_1_tail/2, [List]),
	prof_object:run_cpu_profiling(CSeeds, "| lists:append/2 (head)", fun test_op_append_2_head/2, [List]),
	prof_object:run_cpu_profiling(CSeeds, "| lists:append/2 (tail)", fun test_op_append_2_tail/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "R erlang:length/1", fun test_op_length/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "| lists:keyfind/3", fun test_op_keyfind/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "| lists:keysearch/3", fun test_op_keysearch/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "| lists:member/2", fun test_op_member/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "| erlang:hd/1", fun test_op_hd/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "| erlang:tl/1", fun test_op_tl/2, [List]),
	prof_object:run_cpu_profiling(RSeeds, "| lists:last/1", fun test_op_last/2, [List]),
	prof_object:run_cpu_profiling(RIndexes, "| lists:nth/2", fun test_op_nth/2, [List]),
	prof_object:run_cpu_profiling(RIndexes, "| lists:nthtail/2", fun test_op_nthtail/2, [List]),
	prof_object:run_cpu_profiling(USeeds, "U lists:keystore/4 (exist)", fun test_op_keystore/2, [List]),
	prof_object:run_cpu_profiling(USeeds, "| lists:keyreplace/4", fun test_op_keyreplace/2, [List]),
	prof_object:run_cpu_profiling(DSeeds, "D lists:keydelete/3", fun test_op_keydelete/2, [List]),
	prof_object:run_cpu_profiling(DSeeds, "| lists:delete/2", fun test_op_delete/2, [List]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_memory(List) ->
	Seed = 0,
	Elem = {?KEY(Seed), ?VAL(Seed)},
	WordSize = erlang:system_info(wordsize),
	Size = erts_debug:size(Elem) * WordSize,
	FlatSize = erts_debug:flat_size(Elem) * WordSize,
	TotalSize = erts_debug:size(List) * WordSize,
	TotalFlatSize = erts_debug:flat_size(List) * WordSize,
	{Size, FlatSize, TotalSize, TotalFlatSize}.

test_zero_load(_Seed, _Args) ->
	ok.

test_op_keystore(Seed, [List]) ->
	lists:keystore(?KEY(Seed), 1, List, {?KEY(Seed), ?VAL(Seed)}).

test_op_append_1_head(Seed, [List]) ->
	lists:append([[?ELEM(Seed)], List]).

test_op_append_1_tail(Seed, [List]) ->
	lists:append([List, [?ELEM(Seed)]]).

test_op_append_2_head(Seed, [List]) ->
	lists:append([?ELEM(Seed)], List).

test_op_append_2_tail(Seed, [List]) ->
	lists:append(List, [?ELEM(Seed)]).

test_op_length(_Seed, [List]) ->
	erlang:length(List).

test_op_keyfind(Seed, [List]) ->
	lists:keyfind(?KEY(Seed), 1, List).

test_op_keysearch(Seed, [List]) ->
	lists:keysearch(?KEY(Seed), 1, List).

test_op_member(Seed, [List]) ->
	lists:member(?ELEM(Seed), List).

test_op_hd(_Seed, [List]) ->
	erlang:hd(List).

test_op_tl(_Seed, [List]) ->
	erlang:tl(List).

test_op_last(_Seed, [List]) ->
	lists:last(List).

test_op_nth(Seed, [List]) ->
	lists:nth(Seed, List).

test_op_nthtail(Seed, [List]) ->
	lists:nthtail(Seed, List).

test_op_keyreplace(Seed, [List]) ->
	lists:keyreplace(?KEY(Seed), 1, List, {?KEY(Seed), ?VAL(Seed)}).

test_op_keydelete(Seed, [List]) ->
	lists:keydelete(?KEY(Seed), 1, List).

test_op_delete(Seed, [List]) ->
	lists:delete(?ELEM(Seed), List).

set_background_data(Data) ->
	Data.

generate_background_data(Seeds) ->
	[{?KEY(S), ?VAL(S)} || S <- Seeds].

generate_indexes(Seeds, Length) ->
	generate_indexes_help(Seeds, Length, []).

generate_indexes_help([_Seed | RestSeeds], Length, Indexes) ->
	Index = rand:uniform(Length),
	generate_indexes_help(RestSeeds, Length, [Index | Indexes]);

generate_indexes_help([], _Length, Indexes) ->
	Indexes.