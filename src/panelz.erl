%%% **************************************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% @doc A panel defines an area or "block" of content on an html page. A panel record 
%%% provides metadata and an erlang mfa call that executes content.
%%% 
%%% Metadata includes: id, css class, span in Twitter Bootstrap units, offset in
%%% Twitter Bootstrap units, and site id.
%%%
%%% NOTE Bootstrap constraints: ?MAXSPAN must be equal to or grater than span + offset
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


-module(panelz).

-export([create_panel/0, create_panel/1, create_panel/2, available/1, create_panel/3]).
-export([get_record_type/1, is_panel/1, its_me/2, confirm/2]).
-export([get_id/1, put_id/2, update_id/3, i_am/1]).
-export([get_class/1, put_class/2, update_class/3]).
-export([get_width/1, get_width/2, get_width_spec/1, get_width_spec/2]).
-export([is_width_spec_valid/2, put_width/3, edit_width/4]).
-export([get_span/1, put_span/2, edit_span/3]).
-export([get_offset/1, put_offset/2, edit_offset/3]).
-export([get_mfa/1, put_mfa/4, edit_mfa/5]). 
-export([put_dummy_content/2, edit_dummy_content/3]).
-export([display/1, to_iolist/1, spaces/1, to_html/2]). 

%% -compile(export_all).

-include("../include/grid.hrl").


%% ************************************************************
%% Panel functions - create/0
%% ************************************************************


%% @doc Create default panel
-spec create_panel() -> tuple().

create_panel() ->
   #panel{}.


%% ************************************************************
%%  create/1
%% ************************************************************


%% @doc Create panel with defined id
-spec create_panel(ID::list()) -> tuple().

create_panel(Panel_ID) ->
   #panel{panel_id = Panel_ID}.


%% ************************************************************
%% create_panel/2
%% ************************************************************


%% @doc Create panel with id and span
-spec create_panel(Panel_ID::list(), Span::integer()) -> tuple().

create_panel(Panel_ID, Span) ->
   #panel{panel_id = Panel_ID, span = Span}.


%% ************************************************************
%% available/1 
%% ************************************************************


%% @doc Return number of width units available for expansion
-spec available(Panel::tuple()) -> integer().

available(Panel) ->
   #panel{span=Span, offset=Offset} = Panel,
   ?MAXSPAN - (Span + Offset).


%% ************************************************************
%% create_panel/3 
%% ************************************************************


%% @doc If space available, create panel data structure, including span value
-spec create_panel(Panel_ID::list(), Span::integer(), Available::integer()) -> tuple().

create_panel(Panel_ID, Span, Available) ->
   Panel = create_panel(Panel_ID),
   case Span =< Available of
      true  -> Panel#panel{span=Span};
      false -> Panel
   end.
 

%% ************************************************************
%% get_record_type/1 
%% ************************************************************


%% @doc Return record type
-spec get_record_type(Record::tuple()) -> atom().

get_record_type(Record) ->
   element(1, Record).


%% ************************************************************
%% is_panel/1 
%% ************************************************************


%% @doc Return true if record is a panel
-spec is_panel(Record::tuple()) -> boolean().

is_panel(Record) ->
   case get_record_type(Record) of
      panel -> true;
      _     -> false
   end.

   
%% ************************************************************
%% its_me/2 
%% ************************************************************


%% @doc Return true if ID matches id of panel 
-spec its_me(Record::tuple(), ID::list()) -> boolean().

its_me(Record, ID) ->
   Flag = get_record_type(Record),
   case Flag of
      panel  -> panelz:get_id(Record) == ID;
      row -> false
   end.


%% ************************************************************
%% confirm/2 
%% ************************************************************


%% @doc Verify identity of panel  
 -spec confirm(Panel::tuple(), Confirm_ID::list()) -> boolean().

confirm(Panel, Confirm_ID) ->
  ID = get_id(Panel),
  ID == Confirm_ID.


%% ************************************************************
%% get_id/1 
%% ************************************************************


%% @doc Return panel id 
-spec get_id(Panel::tuple()) -> integer() | list().

get_id(Panel) ->
   Panel#panel.panel_id.


%% ************************************************************
%% put_id/2 
%% ************************************************************


%% @doc Store panel ID 
-spec put_id(Panel::tuple(), Panel_Id::list()) -> tuple().

put_id(Panel, Panel_ID) -> 
   Panel#panel{panel_id=Panel_ID}.


%% ************************************************************
%% update_id/3 
%% ************************************************************


%% @doc If panel verified, update panel ID
-spec update_id(Panel::tuple(), Old_ID::list(), New_ID::list()) -> tuple().

update_id(Panel, Old_ID, New_ID) ->
   case confirm(Panel, Old_ID) of
      true  -> put_id(Panel, New_ID);
      false -> Panel
   end.


%% ************************************************************
%% i_am/1 
%% ************************************************************


%% @doc Display panel id
-spec i_am(Panel_ID::list()) -> list().

i_am(Panel_ID) ->
   io:format("panel: ~s~n", [Panel_ID]).


%% ************************************************************
%% get_class/1 
%% ************************************************************


%% @doc Return css class
-spec get_class(Panel::tuple()) -> list().

get_class(Panel) ->
   Panel#panel.class.


%% ************************************************************
%% put_class/2 
%% ************************************************************


%% @doc Store panel class
-spec put_class(Panel::tuple(), Class::list()) -> tuple().

put_class(Panel, Class) when is_tuple(Panel), is_list(Class) ->
   case Class == "span" of
      true  -> Panel#panel{class=Class, span=1};
      false -> Panel#panel{class=Class, span=0}
   end.


%% ************************************************************
%% update_class/3 
%% ************************************************************


%% @doc If panel verified, update class
-spec update_class(Panel::tuple(), Confirm_ID::list(), Class::list()) -> tuple().

update_class(Panel, Confirm_ID, Class) ->
   case confirm(Panel, Confirm_ID)  of
      true  -> put_class(Panel, Class);
      false -> Panel
   end.


%% ************************************************************
%% get_width/1 
%% NOTE: width = span + offset
%% ************************************************************


%% @doc Return width of panel (span + offset)
-spec get_width(Panel::tuple()) -> integer().

get_width(Panel) ->
   #panel{span=Span, offset=Offset} = Panel,
   Span + Offset.


%% ************************************************************
%% get_width/2 
%% ************************************************************


%% @doc If panel verified, return total panel width (span + offset) 
-spec get_width(Panel::tuple(), Confirm_ID::list()) -> integer().

get_width(Panel, Confirm_ID) ->
   case confirm(Panel, Confirm_ID) of
      true  -> get_width(Panel);
      false -> 0
   end.


%% ************************************************************
%% get_width_spec/1 
%% ************************************************************


%% @doc Return span and offset of panel
-spec get_width_spec(Panel::tuple()) -> tuple().

get_width_spec(Panel) ->
   #panel{span=Span, offset=Offset} = Panel,
   {Span, Offset}.


%% ************************************************************
%% get_width_spec/2 
%% ************************************************************


%% @doc If panel is varified, return span and offset
-spec get_width_spec(Panel::tuple(), Confirm_ID::list()) -> tuple().

get_width_spec(Panel, Confirm_ID) ->
   case confirm(Panel, Confirm_ID) of
      true  -> #panel{span=Span, offset=Offset} = Panel,
              {Span, Offset};
      false -> {0,0}
   end.


%% ************************************************************
%% is_width_spec_valid/2 
%% ************************************************************


%% @doc Return true if Span greater than 0 and Offset greater than or equal to 0 
-spec is_width_spec_valid(Span::integer(), Offset::integer()) -> boolean().

is_width_spec_valid(Span, Offset) ->
    (Span > 0) and (Offset >= 0) and (Span + Offset =< ?MAXSPAN).


%% ************************************************************
%% put_width/3 
%% ************************************************************


%% @doc Unconditionally update panel width 
-spec put_width(Panel::tuple(), Span::integer(), Offset::integer()) -> tuple().

put_width(Panel, Span, Offset) ->
   case is_width_spec_valid(Span, Offset) of
      true  -> Panel#panel{span=Span, offset=Offset};
      false -> Panel
   end.


%% ************************************************************
%% edit_width/4 
%% ************************************************************


%% @doc If panel verified, update panel width
-spec edit_width(Panel::tuple(), Confirm_ID::integer()|binary(), Span::integer(), Offset::integer()) -> tuple().

edit_width(Panel, Confirm_ID, Span, Offset) ->
   case confirm(Panel, Confirm_ID) of
      true  -> put_width(Panel, Span, Offset);
      false -> Panel
   end.


%% ************************************************************
%% get_span/1 
%% ************************************************************


%% @doc Return span 
-spec get_span(Panel::tuple()) -> integer().

get_span(Panel) ->
   Panel#panel.span.


%% ************************************************************
%% put_span/2 
%% ************************************************************


%% @doc Update span 
-spec put_span(Panel::tuple(), Span::integer()) -> tuple().

put_span(Panel, Span) ->
   Offset = get_offset(Panel),
   put_width(Panel, Span, Offset).


%% ************************************************************
%% edit_span/3 
%% ************************************************************


%% @doc If panel verified, update span 
-spec edit_span(Panel::tuple(), ID::integer()|binary(), Span::integer()) -> tuple().


edit_span(Panel, Confirm_ID, Span) ->
   case confirm(Panel, Confirm_ID) of
      true  -> put_span(Panel, Span);
      false -> Panel
   end.


%% ************************************************************
%% get_offset/1 
%% ************************************************************


%% @doc Return panel offset 
-spec get_offset(Panel::tuple()) -> integer().

get_offset(Panel) ->
   Panel#panel.offset.


%% ************************************************************
%% put_offset/2 
%% ************************************************************


%% @doc Update offset  
-spec put_offset(Panel::tuple(), Offset::integer()) -> tuple().

put_offset(Panel, Offset) ->
   Span = get_span(Panel),
   put_width(Panel, Span, Offset).



%% ************************************************************
%% edit_offset/2 
%% ************************************************************


%% @doc If panel verified, edit offset  
-spec edit_offset(Panel::tuple(), ID::list(), Offset::list()) -> tuple().

edit_offset(Panel, ID, Offset) ->
   case confirm(Panel, ID) of
      true  -> put_offset(Panel, Offset);
      false -> Panel
   end.


%% ************************************************************
%% get_mfa/1 
%% ************************************************************


%% @doc Return content function
-spec get_mfa(Panel::tuple()) -> tuple().

get_mfa(Panel) ->
   #panel{module=Module, function=Function, parameters=Parameters} = Panel,
   {Module, Function, Parameters}.


%% ************************************************************
%% put_mfa/4 
%% ************************************************************


%% @doc Update content function
-spec put_mfa(Panel::tuple(), Module::atom(), Function::atom(), Parameters::list()) -> tuple().

put_mfa(Panel, Module, Function, Parameters) ->
   Panel#panel{module=Module, function=Function, parameters=Parameters}.


%% ************************************************************
%% edit_mfa/4 
%% ************************************************************


%% @doc If panel verified, update content function
-spec edit_mfa(Panel::tuple(), Confirm_ID::list(), Module::atom(), Function::atom(), Parameters::list()) -> tuple().

edit_mfa(Panel, Confirm_ID, Module, Function, Parameters) ->
   case confirm(Panel, Confirm_ID) of
      true  -> put_mfa(Panel, Module, Function, Parameters);
      false -> Panel
   end.


%% ************************************************************
%% put_dummy_content/2 
%% ************************************************************


%% @doc Update dummy content function
-spec put_dummy_content(Panel::tuple(), Token::atom) -> tuple().

put_dummy_content(Panel, Token) ->
   put_mfa(Panel, clipz, dummy, [Token]).
   

%% ************************************************************
%% edit_dummy_content/3 
%% ************************************************************


%% @doc If panel verified, update dummy content
-spec edit_dummy_content(Panel::tuple(), Panel_ID::list(), Token::atom) -> tuple().

edit_dummy_content(Panel, Panel_ID, Token) ->
   case confirm(Panel, Panel_ID) of
      true  -> put_dummy_content(Panel, Token);
      false -> Panel
   end.


%% ************************************************************
%% display/1 
%% ************************************************************


%% @doc Display panel with dummy content
-spec display(Panel::tuple()) -> iolist().

display(Panel) ->
  Id = get_id(Panel),
  {M,F,A} = get_mfa(Panel),
  case {M, F, A}  of
     {_, dummy, [id]}       -> clipz:dummy_id(Id);
     {_, dummy, [masthead]} -> clipz:dummy_masthead(Id);
     {_, dummy, [nav]}      -> clipz:dummy_nav(Id);
     {_, dummy, [squib]}    -> clipz:dummy_squib(Id);
     {_, dummy, [clip]}     -> clipz:dummy_clip(Id);
     {_, dummy, [article]}  -> clipz:dummy_article(Id);
     {_, dummy, [box]}      -> clipz:dummy_box(Id);
     {_, dummy, [pic]}      -> clipz:dummy_pic(Id);
     {_, dummy, [footer]}   -> clipz:dummy_footer(Id);
     {undefined, _ , _}     -> clipz:dummy_id(Id);
     {_,_,_}                -> erlang:apply(M, F, A)
   end.


%% ************************************************************
%% class/3 
%% ************************************************************


class(Class, Span, Offset) ->
   if
      Class == "span" ->
         " class = \"" ++ "span" ++ integer_to_list(Span) ++ "\"";
      Class == "span" and (Offset > 0) ->
         O = "offset" ++ integer_to_list(Offset),
         " class = \"" ++ "span" ++ integer_to_list(Span) ++ " " ++ O ++ "\"";
      true            ->
         " class = \"" ++ Class ++ "\""
   end.


%% ************************************************************
%% to_iolist/1
%% ************************************************************


%% @doc Format grid as i/o list 
-spec to_iolist(Panel::tuple()) -> iolist().

to_iolist(Panel) ->
  #panel{panel_id=ID, class=Class, span=Span, offset=Offset} = Panel,
  Class1 = class(Class, Span, Offset),
  ["<div id=", "\"", ID, "\"", Class1,">\n",
  panelz:display(Panel),
  "</div>\n\n"].


%% ************************************************************
%% helper - to_html/2
%% ************************************************************


%% @doc Return binary string of N spaces
-spec spaces(N::integer()) -> binary().

spaces(N) ->
   list_to_binary(string:chars($ , N)).


%% ************************************************************
%% to_html/2
%% ************************************************************


%% @doc Format panel as html 
-spec to_html(Panel::tuple(), Indent::integer()) -> iolist().

to_html(Panel, Indent) ->
  #panel{class=Class, span=Span, offset=Offset} = Panel,
  Class1 = class(Class, Span, Offset),
  IOList = [spaces(Indent + 3), "<div ", Class1, ">\n",
  spaces(Indent + 6), panelz:display(Panel), spaces(Indent + 3),
  "</div>\n"],
   io:format("~s~n", [IOList]).


     

