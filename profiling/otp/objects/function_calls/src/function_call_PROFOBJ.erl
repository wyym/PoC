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
%%%   This module is the profiling object for profiling different ways for
%%%   function call.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(function_call_PROFOBJ).

-namespace('eppp.objects.function_calls').

-visibility(public).

-vsn(20191023).


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
%%% Exported Functions for Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
	do_nothing/1
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Include Files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("prof_object.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Registration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?REG_PROFOBJ("function-calls").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiles Function Call Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PROFILES, [
	{round, 10000000}  %% 10M rounds/operation
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External Module Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EXTERNAL_MODULE, fake_external_module).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function Name Definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FUNCTION, do_nothing).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profiling Object Behavioral Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prepare() -> [{round, Rounds::non_neg_integer()}].
prepare() ->
	?PROFILES.

-spec run(Rounds::non_neg_integer()) -> any().
run(Rounds) ->
	prof_object:run_cpu_profiling(Rounds, "No operation (zero load)", fun test_zero_load/2, [?MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "f(A) (local)", fun test_fa_local_known_function_call/2, [?MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "F(A) (local)", fun test_fa_anonymous_function_call/2, [?MODULE, fun ?FUNCTION/1, arg]),
	prof_object:run_cpu_profiling(Rounds, "F(A) (external)", fun test_fa_anonymous_function_call/2, [?EXTERNAL_MODULE, fun ?EXTERNAL_MODULE:?FUNCTION/1, arg]),
	prof_object:run_cpu_profiling(Rounds, "m:f(A) (local)", fun test_mfa_local_known_module_known_function_call/2, [?MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "m:f(A) (external)", fun test_mfa_external_known_module_known_function_call/2, [?EXTERNAL_MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "M:f(A) (local)", fun test_mfa_anonymous_module_known_function_call/2, [?MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "M:f(A) (external)", fun test_mfa_anonymous_module_known_function_call/2, [?EXTERNAL_MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "M:F(A) (local)", fun test_mfa_anonymous_module_anonymous_function_call/2, [?MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "M:F(A) (external)", fun test_mfa_anonymous_module_anonymous_function_call/2, [?EXTERNAL_MODULE, ?FUNCTION, arg]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(f, A) (local)", fun test_apply2_local_known_function_call/2, [?MODULE, fun ?FUNCTION/1, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(F, A) (local)", fun test_apply2_anonymous_function_call/2, [?MODULE, fun ?FUNCTION/1, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M:F, A) (local)", fun test_apply2_anonymous_function_call/2, [?MODULE, fun ?MODULE:?FUNCTION/1, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M:F, A) (external)", fun test_apply2_anonymous_function_call/2, [?EXTERNAL_MODULE, fun ?EXTERNAL_MODULE:?FUNCTION/1, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(m, f, A) (local)", fun test_apply3_local_known_module_known_function_call/2, [?MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(m, f, A) (external)", fun test_apply3_external_known_module_known_function_call/2, [?EXTERNAL_MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M, f, A) (local)", fun test_apply3_anonymous_module_known_function_call/2, [?MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M, f, A) (external)", fun test_apply3_anonymous_module_known_function_call/2, [?EXTERNAL_MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M, F, A) (local)", fun test_apply3_anonymous_module_anonymous_function_call/2, [?MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M, F, A) (external)", fun test_apply3_anonymous_module_anonymous_function_call/2, [?EXTERNAL_MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M, F, a) (local)", fun test_apply3_anonymous_module_anonymous_function_known_arguments_call/2, [?MODULE, ?FUNCTION, [arg]]),
	prof_object:run_cpu_profiling(Rounds, "erlang:apply(M, F, a) (external)", fun test_apply3_anonymous_module_anonymous_function_known_arguments_call/2, [?EXTERNAL_MODULE, ?FUNCTION, [arg]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_nothing(_Args) ->
	ok.

test_zero_load(_Round, [_M, _F, _A]) ->
	ok.

test_fa_local_known_function_call(_Round, [_M, _F, A]) ->
	?FUNCTION(A).

test_fa_anonymous_function_call(_Round, [_M, F, A]) ->
	F(A).

test_mfa_local_known_module_known_function_call(_Round, [_M, _F, A]) ->
	?MODULE:?FUNCTION(A).

test_mfa_external_known_module_known_function_call(_Round, [_M, _F, A]) ->
	?EXTERNAL_MODULE:?FUNCTION(A).

test_mfa_anonymous_module_known_function_call(_Round, [M, _F, A]) ->
	M:?FUNCTION(A).

test_mfa_anonymous_module_anonymous_function_call(_Round, [M, F, A]) ->
	M:F(A).

test_apply2_local_known_function_call(_Round, [_M, _F, A]) ->
	erlang:apply(fun ?FUNCTION/1, A).

test_apply2_anonymous_function_call(_Round, [_M, F, A]) ->
	erlang:apply(F, A).

test_apply3_local_known_module_known_function_call(_Round, [_M, _F, A]) ->
	erlang:apply(?MODULE, ?FUNCTION, A).

test_apply3_external_known_module_known_function_call(_Round, [_M, _F, A]) ->
	erlang:apply(?EXTERNAL_MODULE, ?FUNCTION, A).

test_apply3_anonymous_module_known_function_call(_Round, [M, _F, A]) ->
	erlang:apply(M, ?FUNCTION, A).

test_apply3_anonymous_module_anonymous_function_call(_Round, [M, F, A]) ->
	erlang:apply(M, F, A).

test_apply3_anonymous_module_anonymous_function_known_arguments_call(_Round, [M, F, _A]) ->
	erlang:apply(M, F, [arg]).