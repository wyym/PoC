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
%%%   This module provides the methods for generating the data collection
%%%   according to the specified arguments.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(data_utils).

-namespace('eppp.fw').

-visibility(public).

-vsn(20190628).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
	generate_within/3,
	generate_without/3
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([
	seed/0
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type seed() :: integer().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Supported Options Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(OPT_NBR_OF_SEGMENTS, number_of_segments).
-define(OPT_GAP_BETWEEN_SEGMENTS, gap_between_segments).
-define(OPT_SEED_OFFSET, seed_offset).
-define(OPT_UNIQUE_REQUIRED, unique_required).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_within(NbrOfSeeds::non_neg_integer(),
	NbrPerSegment::pos_integer(), Options::[Option]) -> [seed()]
	when Option::
		{number_of_segments, pos_integer()} |
		{gap_between_segments, non_neg_integer()} |
		{seed_offset, integer()} |
		{unique_required, boolean()} | unique_required.
generate_within(NbrOfSeeds, NbrPerSegment, Options) ->
	if
		is_integer(NbrOfSeeds), NbrOfSeeds >= 0 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	if
		is_integer(NbrPerSegment), NbrPerSegment >= 1 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	NbrOfSegments = proplists:get_value(?OPT_NBR_OF_SEGMENTS, Options, 1),
	if
		is_integer(NbrOfSegments), NbrOfSegments >= 1 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	GapBetweenSegments = proplists:get_value(?OPT_GAP_BETWEEN_SEGMENTS, Options, 0),
	if
		is_integer(GapBetweenSegments), GapBetweenSegments >= 0 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	SeedOffset = proplists:get_value(?OPT_SEED_OFFSET, Options, 0),
	if
		is_integer(SeedOffset) ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	UniqSeedsRequired = proplists:get_bool(?OPT_UNIQUE_REQUIRED, Options),
	if
		not UniqSeedsRequired; NbrOfSeeds =< NbrPerSegment * NbrOfSegments ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	case UniqSeedsRequired of
		true ->
			generate_unique_seeds_within(NbrOfSeeds, NbrPerSegment,
				NbrOfSegments, GapBetweenSegments, SeedOffset);
		false ->
			generate_random_seeds_within(NbrOfSeeds, NbrPerSegment,
				NbrOfSegments, GapBetweenSegments, SeedOffset)
	end.

-spec generate_without(NbrOfSeeds::non_neg_integer(),
	NbrPerSegment::pos_integer(), Options::[Option]) -> [seed()]
	when Option::
		{number_of_segments, pos_integer()} |
		{gap_between_segments, non_neg_integer()} |
		{seed_offset, integer()} |
		{unique_required, boolean()} | unique_required.
generate_without(NbrOfSeeds, NbrPerSegment, Options) ->
	if
		is_integer(NbrOfSeeds), NbrOfSeeds >= 0 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	if
		is_integer(NbrPerSegment), NbrPerSegment >= 1 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	NbrOfSegments = proplists:get_value(?OPT_NBR_OF_SEGMENTS, Options, 1),
	if
		is_integer(NbrOfSegments), NbrOfSegments >= 1 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	GapBetweenSegments = proplists:get_value(?OPT_GAP_BETWEEN_SEGMENTS, Options, 0),
	if
		is_integer(GapBetweenSegments), GapBetweenSegments >= 0 ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	SeedOffset = proplists:get_value(?OPT_SEED_OFFSET, Options, 0),
	if
		is_integer(SeedOffset) ->
			ok;
		true ->
			erlang:error(badarg, [NbrOfSeeds, NbrPerSegment, Options])
	end,
	UniqSeedsRequired = proplists:get_bool(?OPT_UNIQUE_REQUIRED, Options),
	case UniqSeedsRequired of
		true ->
			generate_unique_seeds_without(NbrOfSeeds, NbrPerSegment,
				NbrOfSegments, GapBetweenSegments, SeedOffset);
		false ->
			generate_random_seeds_without(NbrOfSeeds, NbrPerSegment,
				NbrOfSegments, GapBetweenSegments, SeedOffset)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_random_seeds_without(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset) when GapBetweenSegments > 0 ->
	generate_random_seeds(NbrOfGenSeeds, GapBetweenSegments, NbrOfSegments + 1,
		NbrPerSegment, SeedOffset - GapBetweenSegments);

generate_random_seeds_without(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		_GapBetweenSegments, SeedOffset) ->
	if
		NbrOfGenSeeds >= 2 ->
			NbrPerSegmentA = NbrOfGenSeeds div 2;
		true ->
			NbrPerSegmentA = 1
	end,
	GapBetweenSegmentsA = NbrPerSegment * NbrOfSegments,
	generate_random_seeds(NbrOfGenSeeds, NbrPerSegmentA, 2,
		GapBetweenSegmentsA, SeedOffset - NbrPerSegmentA).

generate_unique_seeds_without(NbrOfGenSeeds, NbrPerSegment,
		NbrOfSegments, GapBetweenSegments, SeedOffset) ->
	TotalSeeds = GapBetweenSegments * (NbrOfSegments + 1),
	if
		NbrOfGenSeeds =< TotalSeeds ->
			generate_unique_seeds(NbrOfGenSeeds, GapBetweenSegments,
				NbrOfSegments + 1, NbrPerSegment, SeedOffset - GapBetweenSegments);
		true ->
			Seeds1 = generate_unique_seeds(TotalSeeds, GapBetweenSegments,
				NbrOfSegments + 1, NbrPerSegment, SeedOffset - GapBetweenSegments),
			NbrOfLarge = (NbrOfGenSeeds - TotalSeeds + 1) div 2,
			LargeBegin = (NbrPerSegment + GapBetweenSegments) * NbrOfSegments,
			LargeEnd = LargeBegin + NbrOfLarge - 1,
			Seeds2 = generate_seeds(LargeBegin, LargeEnd, SeedOffset, Seeds1),
			NbrOfSmall = NbrOfGenSeeds - TotalSeeds - NbrOfLarge,
			SmallBegin = -GapBetweenSegments - NbrOfSmall,
			SmallEnd = SmallBegin + NbrOfSmall - 1,
			generate_seeds(SmallBegin, SmallEnd, SeedOffset, Seeds2)
	end.

generate_random_seeds_within(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset) ->
	generate_random_seeds(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset).

generate_unique_seeds_within(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset) ->
	generate_unique_seeds(NbrOfGenSeeds, NbrPerSegment,
		NbrOfSegments, GapBetweenSegments, SeedOffset).

generate_random_seeds(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset) ->
	generate_random_seeds_help(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset, []).

generate_random_seeds_help(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset, Seeds) when NbrOfGenSeeds > 0 ->
	SegmentIndex = rand:uniform(NbrOfSegments) - 1,
	InSegmentIndex = rand:uniform(NbrPerSegment) - 1,
	Seed = (NbrPerSegment + GapBetweenSegments) * SegmentIndex
		+ InSegmentIndex + SeedOffset,
	generate_random_seeds_help(NbrOfGenSeeds - 1, NbrPerSegment,
		NbrOfSegments, GapBetweenSegments, SeedOffset, [Seed | Seeds]);

generate_random_seeds_help(_NbrOfGenSeeds, _NbrPerSegment, _NbrOfSegments,
		_GapBetweenSegments, _SeedBeginOffset, Seeds) ->
	Seeds.

generate_unique_seeds(NbrOfGenSeeds, NbrPerSegment, NbrOfSegments,
		GapBetweenSegments, SeedOffset) ->
	InitSegmentIndex = NbrOfSegments div 2,
	generate_unique_seeds_help(NbrOfGenSeeds, InitSegmentIndex, 0,
		NbrPerSegment, NbrOfSegments, GapBetweenSegments, SeedOffset, []).

generate_unique_seeds_help(NbrOfGenSeeds, SegmentIndex, InSegmentIndex,
		NbrPerSegment, NbrOfSegments, GapBetweenSegments, SeedOffset,
		Seeds) when NbrOfGenSeeds > 0 ->
	Seed = (NbrPerSegment + GapBetweenSegments) * SegmentIndex
		+ InSegmentIndex + SeedOffset,
	NextSegmentIndex = (SegmentIndex + 1) rem NbrOfSegments,
	if
		NextSegmentIndex =:= 0 ->
			NextInSegmentIndex = (InSegmentIndex + 1) rem NbrPerSegment;
		true ->
			NextInSegmentIndex = InSegmentIndex
	end,
	generate_unique_seeds_help(NbrOfGenSeeds - 1, NextSegmentIndex,
		NextInSegmentIndex, NbrPerSegment, NbrOfSegments, GapBetweenSegments,
		SeedOffset, [Seed | Seeds]);

generate_unique_seeds_help(_NbrOfGenSeeds, _SegmentIndex, _InSegmentIndex,
		_NbrPerSegment, _NbrOfSegments, _GapBetweenSegments, _SeedBeginOffset,
		Seeds) ->
	Seeds.

generate_seeds(Begin, End, Offset, Seeds) when Begin =< End ->
	Seed = End + Offset,
	generate_seeds(Begin, End - 1, Offset, [Seed | Seeds]);

generate_seeds(_Begin, _End, _Offset, Seeds) ->
	Seeds.
