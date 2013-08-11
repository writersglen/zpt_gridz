%%% **************************************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Associates key (Page_ID) with process that holds the current working
%%% value of the grid structure
%%%
%%% NOTE: zpt_gridz is inspired by and uses code based on Martin Logan, Erric Merrit,
%%% and Richard Carlson's Simple Cache -- Erlang and OTP in Action, Chapter 6.
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% **************************************************************************************


-module(zpt_gridz_store).

-export([init/0,
         insert/2,
         delete/1,
         lookup/1
       ]).

-define(TABLE_ID, ?MODULE).


%% @doc Initialize key/value store
% -spec init() -> tuple().

init() ->
   ets:new(?TABLE_ID, [public, named_table]).


%% @doc Insert key/value into store
-spec insert(Key::list(), Pid::pid()) -> tuple().

insert(Key, Pid) ->
   ets:insert(?TABLE_ID, {Key, Pid}).


%% @doc Given key, return PID from store
-spec lookup(Key::list()) -> tuple().
 
lookup(Key) ->
   case ets:lookup(?TABLE_ID, Key) of
      [{Key, Pid}] -> {ok, Pid};
      []           -> {error, not_found}
   end.


%% @doc Given PID, delete key/value from store
-spec delete(Pid::pid()) -> tuple().
 
delete(Pid) ->
   ets:match_delete(?TABLE_ID, {'_', Pid}).


