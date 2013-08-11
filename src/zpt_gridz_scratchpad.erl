%%% **************************************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% @doc Store work-in-progress grid edits. 
%%%
%%% NOTE: zpt_gridz is inspired by and uses code based on Martin Logan, Erric Merrit,
%%% and Richard Carlson's Simple Cache -- Erlang and OTP in Action, Chapter 6.
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% **************************************************************************************


-module(zpt_gridz_scratchpad).

-behaviour(gen_server).

-export([ start_link/2,
          create/2,
          create/1,
          fetch/1,
          replace/2,
          delete/1
       ]).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2,
         code_change/3
       ]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).


start_link(Value, LeaseTime) ->
   gen_server:start_link(?MODULE, [Value, LeaseTime], []).


create(Value, LeaseTime) ->
   zpt_gridz_sup:start_child(Value, LeaseTime).


create(Value) ->
  create(Value, ?DEFAULT_LEASE_TIME).


fetch(Pid) ->
   gen_server:call(Pid, fetch).


replace(Pid, Value) ->
   gen_server:cast(Pid, {replace, Value}).


delete(Pid) ->
   gen_server:cast(Pid, delete).


init([Value, LeaseTime]) ->
   Now = calendar:local_time(),
   StartTime = calendar:datetime_to_gregorian_seconds(Now),
   {ok, 
   #state{value = Value, 
          lease_time = LeaseTime,
          start_time = StartTime},
   time_left(StartTime, LeaseTime)}.


time_left(_StartTime, infinity) ->
   infinity;

time_left(StartTime, LeaseTime) ->
   Now = calendar:local_time(),
   CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
   TimeElapsed = CurrentTime - StartTime,
   case LeaseTime - TimeElapsed of
      Time when Time =< 0 -> 0;
      Time                -> Time * 1000
   end.


handle_call(fetch, _From, State) ->
   #state{value = Value, 
          lease_time = LeaseTime,
          start_time = StartTime} = State, 
   TimeLeft = time_left(StartTime, LeaseTime),
   {reply, {ok, Value}, State, TimeLeft}.


handle_cast({replace, Value}, State) ->
   #state{lease_time = LeaseTime,
          start_time = StartTime} = State, 
   TimeLeft = time_left(StartTime, LeaseTime),
   {noreply, State#state{value = Value}, TimeLeft};

handle_cast(delete, State) ->
   {stop, normal, State}.


handle_info(timeout, State) ->
   {stop, normal, State}.


terminate(_Reason, _State) ->
   zpt_gridz_store:delete(self()),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.






