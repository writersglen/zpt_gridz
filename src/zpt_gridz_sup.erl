%%% **************************************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% @doc Supervisor for zpt_gridz
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


-module(zpt_gridz_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_child(Value, LeaseTime) ->
   supervisor:start_child(?SERVER, [Value, LeaseTime]).


init([]) ->
   Grid            = {zpt_gridz_scratchpad, {zpt_gridz_scratchpad, start_link, []},
                     temporary, brutal_kill, worker, [zpt_gridz_scratchpad]},
   Children        =  [Grid],
   RestartStrategy = {simple_one_for_one, 0, 1},
                     {ok, {RestartStrategy, Children}}.


