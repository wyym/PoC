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
%%%   This is the header file defines the method for a ordinary Erlang module
%%%   to be registered as a Profiling Object.
%%% 
%%%   HOW TO CREATE A CONCRETE PROFILING OBJECT?
%%% 
%%%   1) Create a new Erlang module. Normally it is named "<Name>_PROFOBJ.erl".
%%% 
%%%   2) Include this header file as below:
%%% 
%%%      -include("prof_object.hrl").
%%% 
%%%   3) Register the module as the profiling object like below:
%%% 
%%%        ?REG_PROFOBJ(Name).
%%% 
%%%      "Name" is the name of the profiling object and it should be a string.
%%% 
%%%   4) Add the behavior attribute:
%%% 
%%%        -behavior(prof_object).
%%% 
%%%      And implement below callback functions as the concrete profiling
%%%      object and export them:
%%% 
%%%        prepare() -> Profiles.
%%%          Profiles = [Profile]
%%%            Profile = {seed, prof_object:seed_driven_profile()} |
%%%                      {round, Rounds}
%%%                Rounds = non_neg_integer()
%%% 
%%%        run(SeedSetOrRounds) -> any()
%%%          SeedSetOrRounds = prof_object:profiling_seed_set() |
%%%                            non_neg_integer()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(PROF_OBJECT_HRL).
-define(PROF_OBJECT_HRL, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Registration Key Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PROFOBJ_REG_KEY, profiling_object).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Method for Profiling Object Registration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(REG_PROFOBJ(Name), -?PROFOBJ_REG_KEY(Name)).


-endif. %% PROF_OBJECT_HRL
