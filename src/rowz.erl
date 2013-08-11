%%% *************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% @doc Helper functions for zpt_gridz. Rows define a row of 
%%% panels
%%%
%%% Data structure: -record(row, {class= "span", span=12).
%%% NOTE: The span field is included to simplify travesal of
%%% the grid structure.
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
%%% *************************************************************


-module(rowz).

%% row helper functions
-export([its_mine/2]).
-export([get_class/1, put_class/2, get_span/1, put_span/2]).
-export([get_panels/1, put_panels/2]).

%% row functions 
-export([new/0, create_row/1, create_row/2, create_row/3, delete_row/2]). 
-export([stack/1, stack/2, stack/3, nest/3, fluid/1, fixed/1]).
%% panel functions
-export([partition/4, subdivide/3, merge/2, update_panel_id/3]). 
%% content function
-export([dummy_clip/3]).
%% grid functions
-export([signature/1, dimensions/1, to_iolist/1, to_html/1]).

%% -compile([export_all]).

-include("../include/grid.hrl").


%% ************************************************************
%% its_mine/2 
%% ************************************************************


%% @doc Returns true if Panel_ID identifies a child of Record 
-spec its_mine(Record::tuple(), Panel_ID::list()) -> boolean()|tuple().

its_mine(Record, Panel_ID) ->
   Type = panelz:get_record_type(Record),
   case Type of
      panel  -> false;
      row -> Panels = get_panels(Record),
             lists:keymember(Panel_ID, 2, Panels)
   end.




%% ************************************************************
%% get_class/1
%% ************************************************************


%% @doc Return class of row 
-spec get_class(Row::tuple()) -> integer().

get_class(Row) ->
   Row#row.class.


%% ************************************************************
%% put_class/2 
%% ************************************************************


%% @doc update class 
-spec put_class(Row::tuple(), Class::list()) -> tuple().

put_class(Row, Class) ->
   Row#row{class=Class}.


%% ************************************************************
%% get_span/1
%% ************************************************************


%% @doc Return span of row 
-spec get_span(Row::tuple()) -> integer().

get_span(Row) ->
   Row#row.span.


%% ************************************************************
%% put_span/2 
%% ************************************************************


%% @doc update span 
-spec put_span(Row::tuple(), Span::integer()) -> tuple().

put_span(Row, Span) ->
   Row#row{span=Span}.


%% ************************************************************
%% get_panels/1
%% ************************************************************


%% @doc Return children of row 
-spec get_panels(Row::tuple()) -> list().

get_panels(Row) ->
   Row#row.panels.


%% ************************************************************
%% get_panels/1
%% ************************************************************


%% @doc Store list of children in row 
-spec put_panels(Row::tuple(), Panel_List::list()) -> tuple().

put_panels(Row, Panel_List) ->
   Row#row{panels=Panel_List}.


%% ************************************************************
%% do_rows/3
%% ************************************************************


%% @doc Traverse and modify grid
-spec do_rows(Rows::tuple(), Fun::fun(), Params::list()) -> tuple().

do_rows(Rows, Fun, Params) ->
   [do_record(X, Fun, Params) || X <- Rows].


%% ************************************************************
%% do_rows/3 helpers
%% ************************************************************


do_panel(Panel, Fun, Params) ->
   Fun(Panel, Params).


do_record(Record, Fun, Params) ->
   case panelz:get_record_type(Record) of
      row   -> do_row(Record, Fun, Params);
      panel -> do_panel(Record, Fun, Params)
   end.  


do_row(Row, Fun, Params) ->
   Panels = get_panels(Row),
   Result = lists:flatten(do_rows(Panels, Fun, Params)),
   put_panels(Row, Result).


%% ************************************************************
%% new/0 
%% ************************************************************


%% @doc Create row  
-spec new() -> tuple().

new() ->
   Panel = #panel{},
   [#row{panels=[Panel]}].


%% ************************************************************
%% create_row/1 
%% ************************************************************


%% @doc Create default row with one panel; instantiate panel with New_ID 
-spec create_row(New_ID::list()) -> tuple().

create_row(New_ID) when is_list(New_ID) ->
   Panel = panelz:create_panel(New_ID),
   [#row{span=?MAXSPAN, panels=[Panel]}].



%% ************************************************************
%% create_row/2 
%% ************************************************************


%% @doc Create row with one panel; instantiate panel with ID and Span
-spec create_row(New_ID::list(), Span::integer()) -> tuple().

create_row(New_ID, Span) when is_list(New_ID), is_integer(Span) ->
   Panel = #panel{panel_id = New_ID, span = Span},
   [#row{span=Span, panels=[Panel]}].


%% ************************************************************
%% create_row/3 
%% ************************************************************


%% @doc Create row with one panel; instantiate panel with ID, Span, and Offset 
-spec create_row(New_ID::list(), Span::integer(), Offset::integer()) -> tuple().

create_row(New_ID, Span, Offset) when is_list(New_ID), is_integer(Span), is_integer(Offset) ->
   Panel = #panel{panel_id = New_ID, span = Span, offset = Offset},
   [#row{span=Span, panels=[Panel]}].


%% ************************************************************
%% delete_row/2 
%% ************************************************************


%% @doc If row contains panel ID, delete row 
-spec delete_row(Rows::list(), Confirm_ID::list()) -> tuple().

delete_row(Rows, Confirm_ID) ->
   delete_row(Rows, Confirm_ID, []).


%% ************************************************************
%% delete_row helpers 
%% ************************************************************


delete_row(Rows, Confirm_ID, Done) ->
   lists:flatten([maybe_delete_row(X, Confirm_ID, Done) || X <- Rows]).


maybe_delete_row(Record, Confirm_ID, Done) ->
   case panelz:get_record_type(Record) of
      panel -> Record;
      row   -> case its_mine(Record, Confirm_ID) of
                  true  -> [];
                  false -> Panels = get_panels(Record),
                           New_Panels = delete_row(Panels, Confirm_ID, Done),
                           New_Row = put_panels(Record, New_Panels),
                           [New_Row|Done] 
               end
   end. 


%% ************************************************************
%% stack/1 
%% ************************************************************


%% @doc Stack panels to create a grid 
-spec stack(New_IDs::list()) -> tuple().

stack(New_IDs) when is_list(New_IDs) ->
  lists:flatten([create_row(X) || X <- New_IDs]). 


%% ************************************************************
%% stack/2 
%% ************************************************************


%% @doc Stack panels to create a grid 
-spec stack(Span::integer(), New_IDs::list()) -> tuple().

stack(Span, New_IDs) when is_list(New_IDs) ->
   lists:flatten([create_row(X, Span) || X <- New_IDs]).

  
%% ************************************************************
%% stack/3 
%% ************************************************************


%% @doc Stack designated panel on top of a new row
-spec stack(Rows::tuple(), Confirm_ID::list(), New_IDs::list()) -> tuple().

stack(Rows, Confirm_ID, New_IDs) ->
   Params = {Confirm_ID, New_IDs},
   do_rows(Rows, fun stack_me/2, Params).


%% ************************************************************
%% stack/3 - helpers 
%% ************************************************************


stack_me(Record, Params) ->
   {Confirm_ID, New_IDs} = Params,
   case panelz:confirm(Record, Confirm_ID) of
      true  -> #panel{span=Span, offset=Offset} = Record,
               Rows = [create_row(X, Span, Offset) || X <- New_IDs],               
               [Record|Rows];
      false -> Record
   end.

 
%% ************************************************************
%% nest/3 
%% ************************************************************


%% @doc Nest panels to create a grid 
-spec nest(Rows::list(), Confirm_ID::list(), New_ID::list()) -> tuple().

nest(Rows, Confirm_ID, New_ID) ->
   Params = {Confirm_ID, New_ID},
   do_rows(Rows, fun nest/2, Params).


%% ************************************************************
%% nest/3 helper 
%% ************************************************************


nest(Record, Params) ->
   {Confirm_ID, New_ID} = Params,
   case panelz:confirm(Record, Confirm_ID) of
      true  -> #panel{span=Span, offset=Offset} = Record,
               Row = create_row(New_ID, Span, Offset),               
               [Record|Row];
      false -> Record
   end.


%% ************************************************************
%% fluid/1 
%% ************************************************************


%% @doc Configure row as "fluid" (See Twitter Bootstrap for details)
-spec fluid(Row::tuple()) -> tuple(). 


fluid(Rows) ->
   lists:flatten(fluid(Rows, [])).


%% ************************************************************
%% fluid helpers 
%% ************************************************************


fluid(Rows, Done) ->
   lists:flatten([put_fluid(X, Done) || X <- Rows]).


put_fluid(Record, Done) ->
   case panelz:get_record_type(Record) of
      panel -> Record;
      row   ->  Panels = get_panels(Record),
                New_Panels = fluid(Panels, Done),
                New_Row = put_panels(Record, New_Panels),
                New_Row1 = put_class(New_Row, "fluid"),
                [New_Row1|Done] 
   end. 


%% ************************************************************
%% fixed 
%% ************************************************************


%% @doc Configure row as "fixed" (See Twitter Bootstrap for details)
-spec fixed(Row::tuple()) -> tuple(). 

fixed(Rows) ->
   fixed(Rows, []).


%% ************************************************************
%% fixed helpers
%% ************************************************************


fixed(Rows, Done) ->
   lists:flatten([put_fixed(X, Done) || X <- Rows]).


put_fixed(Record, Done) ->
   case panelz:get_record_type(Record) of
      panel -> Record;
      row   ->  Panels = get_panels(Record),
                New_Panels = fixed(Panels, Done),
                New_Row = put_panels(Record, New_Panels),
                New_Row1 = put_class(New_Row, "fixed"),
                [New_Row1|Done] 
   end. 


%% ************************************************************
%% partition/4 
%% ************************************************************


%% @doc Split panel 
-spec partition(Row::tuple(), Confirm_ID::list(), N::integer(), New_ID::list()) -> tuple().

partition(Row, Confirm_ID, N, New_ID) ->
   Params = {Confirm_ID, N, New_ID},
   do_rows(Row, fun partition/2, Params).


%% ************************************************************
%% partition helpers 
%% ************************************************************


partition(Record, Params) ->
  {Confirm_ID, N, New_ID} = Params,
  Flag1 = panelz:its_me(Record, Confirm_ID),
  Span = panelz:get_span(Record),
  Flag2 = (Span > N) and (N > 1),
  case Flag1 and Flag2 of
      true  -> N1 = Span - N,
               Row1 = panelz:put_span(Record, N),
               Row2 = panelz:create_panel(New_ID),
               Row3 = panelz:put_span(Row2, N1),
               [Row1, Row3];
     false -> [Record]
  end.



%% ************************************************************
%% subdivide/3 
%% ************************************************************


%% @doc Divide a panel into N panels where N equals length New_IDs list 
-spec subdivide(Row::tuple(), Confirm_ID::list(), New_IDs::list()) -> tuple().

subdivide(Row, Confirm_ID, New_IDs) ->
   Params = {Confirm_ID, New_IDs},
   do_rows(Row, fun subdivide/2, Params).


%% ************************************************************
%% subdivide/3 - helpers 
%% ************************************************************


subdivide(Panel, Params) ->
   {Confirm_ID, New_IDs} = Params,
   case panelz:confirm(Panel, Confirm_ID) of
      true  -> Span = panelz:get_span(Panel) div length(New_IDs),
               Offset = panelz:get_span(Panel) rem length(New_IDs),
               Panels = [panelz:create_panel(X, Span) || X <- New_IDs],
               [H|T] = Panels,
               H1 = panelz:put_offset(H, Offset),
               [H1|T];
      false -> [Panel]
   end.


%% ************************************************************
%% merge/2 
%% ************************************************************


%% @doc Merge adjacent panels
-spec merge(Row::tuple(), Confirm_ID::list) -> tuple().


merge(Row, Confirm_ID) ->
   do_merge(Row, Confirm_ID).


%% ************************************************************
%% merge/2 helpers 
%% ************************************************************


do_merge(List, Confirm_ID) ->
   [merge_if_row(X, Confirm_ID) || X <- List].


merge_if_row(Record, Confirm_ID) ->
   case panelz:get_record_type(Record) of
      panel -> Record;
      row   -> merge_row(Record, Confirm_ID)
   end.


merge_row(Record, Confirm_ID) ->
   Panels = get_panels(Record),
   Panels1 = merge_panel(Panels, Confirm_ID),
   Panels2 = do_merge(Panels1, Confirm_ID),
   put_panels(Record, Panels2).


maybe_get_id(Record) ->
   case panelz:get_record_type(Record) of
      panel -> panelz:get_id(Record);
      row   -> not_a_panel 
   end.


maybe_merge(Record1, Record2) ->
   Flag1 = panelz:get_record_type(Record1) == panel,
   Flag2 = panelz:get_record_type(Record2) == panel,
   case Flag1 and Flag2 of
      true  -> Span1 = panelz:get_span(Record1),
               Span2 = panelz:get_width(Record2),
               Record1#panel{span = Span1 + Span2};
      false -> [Record1, Record2]
      end.


merge_panel(List, Confirm_ID) ->
   {A,B} = lists:splitwith(fun(X) -> maybe_get_id(X) =/= Confirm_ID end, List),
   case length(B) >= 2 of
      true  -> [X,Y|Rest] = B,
               Merged = maybe_merge(X, Y),
               lists:flatten(A ++ [Merged] ++ Rest);
      false -> A ++ B
    end.


%% ************************************************************
%% update_panel_id/3 
%% ************************************************************


%% @doc If panel in row is verfied, update id of panel   
-spec update_panel_id(Rows::list(), Confirm_ID::list(), New_ID::list()) -> tuple().


update_panel_id(Rows, Confirm_ID, New_ID) ->
   Params = {Confirm_ID, New_ID},
   do_rows(Rows, fun update_panel_id/2, Params).


%% ************************************************************
%% update_panel_id/3 helper 
%% ************************************************************


update_panel_id(Record, Params) ->
   {Confirm_ID, New_ID} = Params,
   case panelz:confirm(Record, Confirm_ID) of
      true  -> io:format("I was once called ~s~n", [Confirm_ID]),
               panelz:put_id(Record, New_ID);
      false -> Record
   end.
  

%% ************************************************************
%% dummy_clip/3 - "id", "masthead", "nav", "squib", "clip", 
%%                "article", "box", "pic", "footer" 
%% ************************************************************


%% @doc If row contains panel with Confirm_ID, update dummy content; 
%% valid tokens: "id", "masthead", "nav", "squib", "clip", "article", 
%% "box", "pic", "footer" 
-spec dummy_clip(Row::tuple(), Confirm_ID::list(), Token::list()) -> tuple().


dummy_clip(Row, Confirm_ID, Token) -> 
   Params = {Confirm_ID, Token},
   do_rows(Row, fun dummy_clip/2, Params).



%% ************************************************************
%% dummy_clip/3 helper 
%% ************************************************************


dummy_clip(Record, Params) ->
   {Confirm_ID, Token} = Params,
   Flag1 = panelz:its_me(Record, Confirm_ID),
   case Flag1 of
      true  -> [panelz:put_dummy_content(Record, Token)];
      false -> [Record]
   end.


%% ************************************************************
%% signature/1 
%% ************************************************************


%% @doc Generate a tag that represents the structure of a row 
-spec signature(Row::tuple()) -> list().

signature(Row) ->
   signature(Row, {0, 0}).


%% ************************************************************
%% signature - helper functions  
%% ************************************************************


inc_panels(Count) ->
  {Panels, Rows} = Count,
  {Panels + 1, Rows}.


inc_rows(Count) ->
   {Panels, Rows} = Count,
   {Panels, Rows + 1}.


signature([], Count) ->
   Count;

signature(Panels, Count) ->
   [Record|Rest] = Panels,
   case panelz:get_record_type(Record) of
      panel -> New_Count = inc_panels(Count),
               signature(Rest, New_Count); 
      row   -> New_Count = inc_rows(Count),
               Panels1 = get_panels(Record),
               New_Count1 = signature(Panels1, New_Count),
               signature(Rest, New_Count1)
   end.


%% ************************************************************
%% dimensions/1   
%% ************************************************************


%% @doc Report number of rows and panels
-spec dimensions(Row::tuple()) -> list().

dimensions(Row) ->
  {Rows, Panels} = signature(Row),
  R = integer_to_list(Rows),
  P = integer_to_list(Panels),
  io:format("~s rows; ~s panels~n", [R, P]).


%% ************************************************************
%% to_iolist/1 
%% ************************************************************


%% @doc Render record as I/0 list   
-spec to_iolist(Record::tuple()) -> list().

to_iolist(Rows) ->
   lists:flatten([render_record(X) || X <- Rows]).


%% ************************************************************
%% to_iolist helpers 
%% ************************************************************


render(Row) ->
  #row{class=Class} = Row,
  Class1 = ["class=\"", Class, "\""],
  ["<div ", Class1, ">\n\n"].


%% @doc Helper function for render/1   
-spec render_record(Record::tuple()) -> list().

render_record(Record) ->
   case panelz:get_record_type(Record) of
      panel  -> panelz:to_iolist(Record);
      row    -> Panels = get_panels(Record),
                lists:append([render(Record), [render_record(X) || X <- Panels], close_row()])
                %% lists:append([render(Record), [render_record(X) || X <- Panels], close_row()])
   end.

close_row() ->
  ["</div>"].



%% ************************************************************
%% to_html/1  
%% ************************************************************


%% @doc Render row to html
-spec to_html(Rows::list()) -> list().

to_html(Rows) ->
   [show_record(X, 0) || X <- Rows].


%% ************************************************************
%% to_html helpers 
%% ************************************************************


row(Row, Indent) ->
  #row{class=Class} = Row,
  Class1 = ["class=\"", Class, "\""],
  L = lists:flatten([panelz:spaces(Indent), "<div ", Class1, ">\n"]),
  io:format("~s~n", [L]).


close_row(Indent) ->
  L = [panelz:spaces(Indent), "</div>\n"],
  io:format("~s~n", [L]).


show_record(Record, Indent) ->
   Type = panelz:get_record_type(Record),
   case Type of
      panel  -> panelz:to_html(Record, Indent);
      row -> to_html(Record, Indent + 4)
   end.


%% Note: Indent facilitates pretty printing the html

to_html(Row, Indent) ->
   row(Row, Indent),
   Panels = get_panels(Row),
   [show_record(X, Indent) || X <- Panels],
   close_row(Indent).


