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
%%%   This module defines how a profiling object looks like, and what kind of
%%%   APIs should be implemented and exported from a profiling object module.
%%% 
%%%   It also provides the API to detect the profiling object modules and the
%%%   common methods for profiling.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(prof_object).

-namespace('eppp.fw').

-visibility(public).

-vsn(20190628).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
	discover/1,
	get_name/1
	]).

-export([
	run_cpu_profiling/4,
	run_mem_profiling/2
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([
	seed_driven_profile/0,
	profiling_seed_set/0
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include Files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("prof_object.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback prepare() -> Profiles::[{seed, seed_driven_profile()} | {round, Rounds::non_neg_integer()}].

-callback run(SeedSetOrRounds::profiling_seed_set() | non_neg_integer()) -> any().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profile and Profiling Seed Set Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type seed_driven_profile() :: {
	NbrPerSegment::pos_integer(),
	NbrOfSegments::pos_integer(),
	GapBetweenSegments::non_neg_integer(),
	NbrOfCrudSeeds::non_neg_integer(),
	UniqueC::boolean(),
	UniqueRUD::boolean()
	}.

-type profiling_seed_set() :: {
	BgSeeds::[data_utils:seed()],
	CSeeds::[data_utils:seed()],
	RSeeds::[data_utils:seed()],
	USeeds::[data_utils:seed()],
	DSeeds::[data_utils:seed()]
	}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec discover(Modules::[module()]) -> [{ProfObjName::string(), Module::module()}].
discover(Modules) ->
	lists:foldr(
		fun(Module, LIn) ->
			case find_profiling_object_attribute(Module) of
				{ok, ProfObjName} ->
					prof_logger:info("detected profiling object ~s, module is ~s~n",
						[ProfObjName, Module]),
					[{ProfObjName, Module} | LIn];
				error ->
					LIn
			end
		end, [], Modules).

-spec get_name(Module::module()) -> ProfObjName::string().
get_name(Module) ->
	case find_profiling_object_attribute(Module) of
		{ok, ProfObjName} ->
			ProfObjName;
		error ->
			erlang:error(badarg, [Module])
	end.

-spec run_cpu_profiling(
		SeedsOrRounds::[term()] | non_neg_integer(),
		OpDescription::string(),
		OpFunc::fun((SeedOrRound::term() | pos_integer(),
			OpArgs::term()) -> any()),
		OpArgs::term()) ->
	ok.
run_cpu_profiling(Rounds, OpDescription, OpFunc, OpArgs) when is_integer(Rounds),
		Rounds >= 0, is_function(OpFunc, 2) ->
	io:format("  ~s: ", [OpDescription]),
	{MicroSecs, _V} = timer:tc(fun run_cpu_profiling_help/4, [1, Rounds, OpFunc, OpArgs]),
	MicroSecsPerOp = MicroSecs / Rounds,
	io:format("~.10B us (~.3f us/round)~n", [MicroSecs, MicroSecsPerOp]);

run_cpu_profiling(Seeds, OpDescription, OpFunc, OpArgs) when is_list(Seeds),
		is_function(OpFunc, 2) ->
	io:format("  ~s: ", [OpDescription]),
	{MicroSecs, _V} = timer:tc(fun run_cpu_profiling_help/3, [Seeds, OpFunc, OpArgs]),
	MicroSecsPerOp = MicroSecs / erlang:length(Seeds),
	io:format("~.10B us (~.3f us/round)~n", [MicroSecs, MicroSecsPerOp]);

run_cpu_profiling(Seeds, OpDescription, OpFunc, OpArgs) ->
	erlang:error(badarg, [Seeds, OpDescription, OpFunc, OpArgs]).

-spec run_mem_profiling(
		MemProfFunc::fun((MemProfArgs::term()) ->
			{Size::non_neg_integer(), FlatSize::non_neg_integer()} |
			{ElemSize::non_neg_integer(), ElemFlatSize::non_neg_integer(),
				TotalSize::non_neg_integer(), TotalFlatSize::non_neg_integer()}),
		MemProfArgs::term()) ->
	ok.
run_mem_profiling(MemProfFunc, MemProfArgs) when is_function(MemProfFunc, 1) ->
	case catch MemProfFunc(MemProfArgs) of
		{Size, FlatSize} when is_integer(Size), is_integer(FlatSize),
				Size >= 0, FlatSize >= 0 ->
			io:format(
				"  Memory Occupation: ~.10B / ~.10B Bytes~n",
				[Size, FlatSize]);
		{ElemSize, ElemFlatSize, TotalSize, TotalFlatSize}
				 when is_integer(ElemSize), is_integer(ElemFlatSize),
				is_integer(TotalSize), is_integer(TotalFlatSize), ElemSize >= 0,
				ElemFlatSize >= 0, TotalSize >= 0, TotalFlatSize >= 0 ->
			io:format(
				"  Memory Occupation per Element: ~.10B / ~.10B Bytes~n"
				"  Total Memory Occupation: ~.10B / ~.10B Bytes~n",
				[ElemSize, ElemFlatSize, TotalSize, TotalFlatSize]);
		{'EXIT', Reason} ->
			erlang:exit(Reason);
		Else ->
			erlang:exit({bad_return_value, Else})
	end;

run_mem_profiling(MemProfFunc, MemProfArgs) ->
	erlang:error(badarg, [MemProfFunc, MemProfArgs]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_cpu_profiling_help(Round, Rounds, OpFunc, OpArgs) when Round =< Rounds ->
	OpFunc(Round, OpArgs),
	run_cpu_profiling_help(Round + 1, Rounds, OpFunc, OpArgs);

run_cpu_profiling_help(_Round, _Rounds, _OpFunc, _OpArgs) ->
	ok.

run_cpu_profiling_help([Seed | RestSeeds], OpFunc, OpArgs) ->
	OpFunc(Seed, OpArgs),
	run_cpu_profiling_help(RestSeeds, OpFunc, OpArgs);

run_cpu_profiling_help([], _OpFunc, _OpArgs) ->
	ok.

find_profiling_object_attribute(Module) ->
	ModAttrs = Module:module_info(attributes),
	case lists:keysearch(?PROFOBJ_REG_KEY, 1, ModAttrs) of
		{value, {?PROFOBJ_REG_KEY, ProfObjName}} ->
			{ok, ProfObjName};
		false ->
			error
	end.
