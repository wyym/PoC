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
%%%   This module is the repository of the profiling objects. It will automated
%%%   detect the available profiling objects from the loaded modules and perform
%%%   the profiling towards the detected profiling objects.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(prof_repo).

-namespace('eppp.app').

-visibility(public).

-vsn(20190628).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
	run/2
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec run(BeamPaths::[string()], ProfObjNames::[string()]) -> any().
run(BeamPaths, ProfObjNames) when is_list(BeamPaths), is_list(ProfObjNames) ->
	Modules = load_beams(BeamPaths),
	DetectedProfObjs = prof_object:discover(Modules),
	SelectedProfObjs = filter_profiling_objects(ProfObjNames, DetectedProfObjs),
	ProfBeginTime = erlang:system_time(milli_seconds),
	ProfResults = run_profiling(SelectedProfObjs),
	ProfEndTime = erlang:system_time(milli_seconds),
	TotalProfDuration = ProfEndTime - ProfBeginTime,
	io:format(
		"----------------------------------------------------------------------~n"
		"Summary~n"
		"----------------------------------------------------------------------~n"
		"Total Profiling Objects: ~.10B~n"
		"Total Profiling Duration: ~.10B ms~n",
		[length(SelectedProfObjs), TotalProfDuration]),
	lists:foreach(
		fun({ProfObj, ProfDuration}) ->
			{ProfObjName, _ProfObjModule} = ProfObj,
			io:format("  ~s Profiling Duration: ~.10B ms~n", [ProfObjName, ProfDuration])
		end, ProfResults).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_profiling(ProfObjs) ->
	run_profiling_help(ProfObjs, []).

run_profiling_help([ProfObj | RestProfObjs], ProfResults) ->
	{ProfObjName, ProfObjModule} = ProfObj,
	io:format(
		"----------------------------------------------------------------------~n"
		"Run profiling for ~s (Module: ~s)~n"
		"----------------------------------------------------------------------~n",
		[ProfObjName, ProfObjModule]),
	case catch ProfObjModule:prepare() of
		Profiles when is_list(Profiles) ->
			ok;
		{'EXIT', Reason} ->
			Profiles = [],
			erlang:exit(Reason);
		Profiles ->
			erlang:exit({bad_return_value, Profiles})
	end,
	ProfBeginTime = erlang:system_time(milli_seconds),
	try
		run_profiling_per_object(ProfObjModule, Profiles)
	catch
		throw:invalid_profile ->
			erlang:exit({bad_return_value, Profiles})
	end,
	ProfEndTime = erlang:system_time(milli_seconds),
	ProfDuration = ProfEndTime - ProfBeginTime,
	run_profiling_help(RestProfObjs, [{ProfObj, ProfDuration} | ProfResults]);

run_profiling_help([], ProfResults) ->
	lists:reverse(ProfResults).

run_profiling_per_object(ProfObjModule, [{seed, {NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, NbrOfSeeds, UniqueC, UniqueRUD}} | RestProfiles]) ->
	BgSeeds = generate_background_data_seeds(NbrPerSegment, NbrOfSegments, GapBetweenSegments),
	{CSeeds, RSeeds, USeeds, DSeeds} = generate_crud_seeds(NbrOfSeeds,
		NbrPerSegment, NbrOfSegments, GapBetweenSegments, UniqueC, UniqueRUD),
	BgCapacity = erlang:length(BgSeeds),
	Rounds = erlang:length(CSeeds),
	io:format("~.10B rounds/operation @ ~.10B Background data:~n", [Rounds, BgCapacity]),
	ProfObjModule:run({BgSeeds, CSeeds, RSeeds, USeeds, DSeeds}),
	run_profiling_per_object(ProfObjModule, RestProfiles);

run_profiling_per_object(ProfObjModule, [{round, Rounds} | RestProfiles]) ->
	io:format("~.10B rounds/operation:~n", [Rounds]),
	ProfObjModule:run(Rounds),
	run_profiling_per_object(ProfObjModule, RestProfiles);

run_profiling_per_object(_ProfObjModule, []) ->
	ok;

run_profiling_per_object(_ProfObjModule, _Profiles) ->
	throw(invalid_profile).

filter_profiling_objects([], ProfObjs) ->
	ProfObjs;

filter_profiling_objects(ProfObjNames, ProfObjs) ->
	ProfObjDict = dict:from_list(ProfObjs),
	lists:foldr(
		fun(ProfObjName, ProfObjListIn) ->
			case dict:find(ProfObjName, ProfObjDict) of
				{ok, ProfObjModule} ->
					[{ProfObjName, ProfObjModule} | ProfObjListIn];
				error ->
					io:format("No profiling object named \"~s\" detected, ignored~n", [ProfObjName]),
					ProfObjListIn
			end
		end, [], ProfObjNames).

load_beams(BeamPaths) ->
	load_beams_from_paths(BeamPaths, []).

load_beams_from_paths([BeamPath | RestBeamPaths], Modules) ->
	case file:list_dir(BeamPath) of
		{ok, BeamFiles} ->
			NewModules = load_beams_from_path(BeamPath, BeamFiles, Modules),
			load_beams_from_paths(RestBeamPaths, NewModules);
		{error, _Err} ->
			prof_logger:warning("cannot access to beam directory ~s, ignore~n", [BeamPath]),
			load_beams_from_paths(RestBeamPaths, Modules)
	end;

load_beams_from_paths([], Modules) ->
	lists:reverse(Modules).

load_beams_from_path(Path, [BeamFile | RestBeamFiles], Modules) ->
	ModName = filename:basename(BeamFile, ".beam"),
	Beam = filename:join(Path, ModName),
	case code:load_abs(Beam) of
		{module, Module} ->
			prof_logger:info("beam file ~s.beam loaded as module ~s~n", [Beam, Module]),
			load_beams_from_path(Path, RestBeamFiles, [Module | Modules]);
		{error, Reason} ->
			prof_logger:warning("cannot load beam file ~s.beam~n due to ~p, ignore~n", [Beam, Reason]),
			load_beams_from_path(Path, RestBeamFiles, Modules)
	end;

load_beams_from_path(_BeamPath, [], Modules) ->
	Modules.

generate_background_data_seeds(NbrPerSegment, NbrOfSegments, GapBetweenSegments) ->
	NbrOfSeeds = NbrPerSegment * NbrOfSegments,
	Options = [
		{number_of_segments, NbrOfSegments},
		{gap_between_segments, GapBetweenSegments},
		unique_required
		],
	data_utils:generate_within(NbrOfSeeds, NbrPerSegment, Options).

generate_crud_seeds(NbrOfSeeds, NbrPerSegment, NbrOfSegments, GapBetweenSegments, UniqueC, UniqueRUD) ->
	COptions = [
		{number_of_segments, NbrOfSegments},
		{gap_between_segments, GapBetweenSegments},
		{unique_required, UniqueC}
		],
	CSeeds = data_utils:generate_without(NbrOfSeeds, NbrPerSegment, COptions),
	RUDOptions = [
		{number_of_segments, NbrOfSegments},
		{gap_between_segments, GapBetweenSegments},
		{unique_required, UniqueRUD}
		],
	RUDSeeds = data_utils:generate_within(NbrOfSeeds, NbrPerSegment, RUDOptions),
	{CSeeds, RUDSeeds, RUDSeeds, RUDSeeds}.
