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
%%%   This file defines the profiles used for different data structures
%%%   profiling.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(DATA_STRUCTURE_PROFILE_DEF_HRL).
-define(DATA_STRUCTURE_PROFILE_DEF_HRL, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiles for ETS Table Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ETS_PROFILES, [
%%  {seed, {NumberPerSegment, NumberOfSegments, GapBetweenSegments, NumberOfCRUDSeeds, UniqueC, UniqueRUD}}
	{seed, {   50, 20, 200, 1000, true, true}}, %% 1K rounds/operation @ 1K data
	{seed, {  500, 20, 200, 1000, true, true}}, %% 1K rounds/operation @ 10K data
	{seed, { 5000, 20, 200, 1000, true, true}}  %% 1K rounds/operation @ 100K data
	% {seed, {50000, 20, 200, 1000, true, true}}  %% 1K rounds/operation @ 1M data
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiles for Other Data Structures Except ETS Table Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NONE_ETS_PROFILES, [
%%  {seed, {NumberPerSegment, NumberOfSegments, GapBetweenSegments, NumberOfCRUDSeeds, UniqueC, UniqueRUD}}
	{seed, {    5, 20, 200, 1000, true, false}}, %% 1K rounds/operation @ 100 data
	{seed, {   50, 20, 200, 1000, true, false}}, %% 1K rounds/operation @ 1K data
	{seed, {  500, 20, 200, 1000, true, false}}, %% 1K rounds/operation @ 10K data
	{seed, { 5000, 20, 200, 1000, true, false}}  %% 1K rounds/operation @ 100K data
	% {seed, {50000, 20, 200, 1000, true, false}}  %% 1K rounds/operation @ 1M data
	]).


-endif. %% DATA_STRUCTURE_PROFILE_DEF_HRL
