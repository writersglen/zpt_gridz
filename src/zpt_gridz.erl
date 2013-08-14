%%% **************************************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% @doc Grid editing functions
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


-module(zpt_gridz).

-export([new/2,
         review/1, 
         clone/2,
         stack/3,
         partition/4,
         subdivide/3,
         update_panel_id/3,
         dummy_clip/3,
         merge/2,
         delete_row/2,
         fluid/1,
         fixed/1,
         to_lib/1,
         to_iolist/1,
         to_html/1,
         to_grid_lib/1,
         delete/1
        ]).


-include("../include/grid.hrl").


%% ************************************************************
%% new/2 
%% ************************************************************


%% @doc Create new grid
-spec new(Page_ID::list(), Panel_IDs::list()) -> tuple().

new(Page_ID, Panel_IDs) when is_list(Page_ID), is_list(Panel_IDs) ->
   Value   = gridz:new(Page_ID, Panel_IDs),
   case zpt_gridz_store:lookup(Page_ID) of
      {ok, _}    -> {error, grid_exists};
      {error, _} -> {ok, Pid} = zpt_gridz_scratchpad:create(Value),
                    zpt_gridz_store:insert(Page_ID, Pid),
                    {ok, Value}
   end.


%% ************************************************************
%% review/2 
%% ************************************************************


%% @doc Return grid
-spec review(Page_ID::list()) -> tuple().

review(Page_ID) when is_list(Page_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      {ok, Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% clone/2 
%% ************************************************************


%% @doc Clone a grid with a new site_id 
-spec clone(Page_ID::list(), Clone_ID::list()) -> tuple().

clone(Page_ID, Clone_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      case zpt_gridz_store:lookup(Clone_ID) of
         {ok, _}    -> {error, grid_exists};
         {error, _} -> {ok, Pid1} = zpt_gridz_scratchpad:create(Value),
                       zpt_gridz_store:insert(Clone_ID, Pid1),
                       {ok, Value}
      end
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% stack/3 
%% ************************************************************


%% @doc Stack one or more  new panels onto an existing panel in grid 
-spec stack(Page_ID::list(), Confirm_ID::list(), Names::list()) -> tuple().

stack(Page_ID, Confirm_ID, Names) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:stack(Value, Confirm_ID, Names),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% partition/4 
%% ************************************************************


%% @doc Partition a panel in grid into two columns; first column has span of N 
-spec partition(Page_ID::list(), Panel_ID::list(), N::integer(), Name::list()) -> tuple().

partition(Page_ID, Panel_ID, N, Name) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:partition(Value, Panel_ID, N, Name),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% subdivide/3 
%% ************************************************************


%% @doc Subdivide a column
-spec subdivide(Page_ID::list(), Confirm_ID::list(), Names::list()) -> tuple().

subdivide(Page_ID, Confirm_ID, Names) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:subdivide(Value, Confirm_ID, Names),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% update_panel_id/3 
%% ************************************************************


%% @doc Update a panel ID
-spec update_panel_id(Page_ID::list(), Confirm_ID::list(), New_ID::list()) -> tuple().

update_panel_id(Page_ID, Confirm_ID, New_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:update_panel_id(Value, Confirm_ID, New_ID),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% dummy_clip/3 
%% ************************************************************


%% @doc Update dummy content; valid tokens: "id", "masthead", "nav", "squib", 
%%      "clip", "article", "box", "pic", "footer" 
-spec dummy_clip(Page_ID::list(), Confirm_ID::list(), Token::list()) -> tuple().

dummy_clip(Page_ID, Confirm_ID, Token) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:dummy_clip(Value, Confirm_ID, Token),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% merge/2 
%% ************************************************************


%% @doc Merge adjacent panels
-spec merge(Page_ID::list(), Confirm_ID::list) -> tuple().

merge(Page_ID, Confirm_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:merge(Value, Confirm_ID),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% delete_row/2 
%% ************************************************************


%% @doc Delete row; will delete all subsidiary panels and rows 
-spec delete_row(Page_ID::list(), Confirm_ID::list) -> tuple().

delete_row(Page_ID, Confirm_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:merge(Value, Confirm_ID),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% fluid/1 
%% ************************************************************


%% @doc Configure fluid grid (See Twitter Bootstrap for details)
-spec fluid(Page_ID::list()) -> tuple().

fluid(Page_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:fluid(Value),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.

%% ************************************************************
%% fixed/1 
%% ************************************************************


%% @doc Configure fixed grid (See Twitter Bootstrap for details)
-spec fixed(Page_ID::list()) -> tuple().

fixed(Page_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      New_Value = gridz:fixed(Value),
      zpt_gridz_scratchpad:replace(Pid, New_Value),
      {ok, New_Value}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% to_lib/1 
%% ************************************************************


%% @doc Save grid to grid library 
-spec to_lib(Page_ID::list()) -> tuple().


to_lib(Page) ->
    Site_ID = "grid_lib",
    Page_ID = zpt_grid_lib:create_id(Page),
    Layout = gridz:get_page_layout(Page),
    Record = #site{site_id = Site_ID, page_id = Page_ID, body = Layout},
    F = fun() ->
                mnesia:write(Record)
        end,
    mnesia:transaction(F).


%% ************************************************************
%% to_iolist/1 
%% ************************************************************


%% @doc Render grid as I/O list 
-spec to_iolist(Page_ID::list()) -> list().

to_iolist(Page_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      gridz:to_iolist(Value)
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% to_html/1 
%% ************************************************************


%% @doc Pretty print grid as html 
-spec to_html(Page_ID::list()) -> list().

to_html(Page_ID) ->
   try
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      gridz:to_html(Value)
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% to_grid_lib/1 
%% ************************************************************


%% @doc Submit grid to grid library
-spec to_grid_lib(Page_ID::list()) -> tuple().

to_grid_lib(Page_ID) ->
   try 
      {ok, Pid} = zpt_gridz_store:lookup(Page_ID),
      {ok, Value} = zpt_gridz_scratchpad:fetch(Pid),
      zpt_grid_lib:insert(Value),
      {ok}
   catch
      _Class:_Exception -> {error, not_found}
   end.


%% ************************************************************
%% delete/1 
%% ************************************************************


%% @doc Delete grid from grid library
-spec delete(Page_ID::list()) -> tuple().

delete(Page_ID) -> 
   case zpt_gridz_store:lookup(Page_ID) of
      {ok, Pid} -> zpt_gridz_scratchpad:delete(Pid),
      zpt_gridz_store:delete(Pid);
      {error, _Reason} -> ok
   end.

     
