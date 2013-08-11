%%% *******************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%% @doc Dummy content to support design of html grids.
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
%%%
%%% *******************************************************


-module(clipz).

-export([spaces/1, dummy_param/1, dummy_id/1, dummy_masthead/1, dummy_nav/1]).
-export([dummy_squib/1, dummy_clip/1, dummy_article/1, dummy_box/1]).
-export([dummy_pic/1, dummy_footer/1, content/1]).

% -compile(export_all).


%% ******************************************************
%% Spaces 
%% ******************************************************


%% @doc Return binary string of N spaces
-spec spaces(N::integer()) -> binary().

spaces(N) ->
   list_to_binary(string:chars($ , N)).


%% ******************************************************
%% Dummy content functions
%% ******************************************************


%% @doc Return valid dummy tag
-spec dummy_param(Tag::list()) -> list().

dummy_param(Tag) ->
   case lists:member(Tag, ["id","masthead","nav","text","box","pic","footer"])  of
      true  -> Tag;
      false -> "text"
   end.


%% @doc Return panel id as html
-spec dummy_id(Panel_ID::list()) -> list().

dummy_id(Panel_ID) ->
   ["<p>panel: ", Panel_ID, "</p>\n"].


%% @doc Return dummy masthead as html
-spec dummy_masthead(Panel_ID::list()) -> list().

dummy_masthead(Panel_ID) ->
  [spaces(6), "<hr />\n<h1>MASTHEAD</h1>\n <h3>panel: ", Panel_ID, "</h3>\n<hr />"].


%% @doc Return dummy nav menu. NOTE: unfinished
-spec dummy_nav(Panel_ID::list()) -> list().

dummy_nav(Panel_ID) ->
  Panel_ID.


%% @doc Return short dummy text squib as html
-spec dummy_squib(Panel_ID::list()) -> list().

dummy_squib(Panel_ID) ->
   [[spaces(6),"<h3>Lorem</h3>\n"],
    [spaces(6),"<h3>panel: ", Panel_ID, "</h3>\n"],
    [spaces(6),"<p> Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore.</p>\n"]].
  
%% @doc Return dummy text clip as html
-spec dummy_clip(Panel_ID::list()) -> list().

dummy_clip(Panel_ID) ->
   [[spaces(6),"<h2>Lorem</h2>\n"],
    [spaces(6),"<h3>panel: ", Panel_ID, "</h3>\n"],
    [spaces(6),"<p> Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.</p>\n"]].


%% @doc Return dummy article as html
-spec dummy_article(Panel_ID::list()) -> list().

dummy_article(Panel_ID) ->
   [[spaces(6),"<h2>Lorem</h2>\n"],
    [spaces(6),"<h3>panel: ", Panel_ID, "</h3>\n"],
    [spaces(6),"<p> Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.</p>\n"],
    [spaces(6),"<h3>Lorem</h3>\n"],
    [spaces(6),"<p> Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.</p>\n"]].


%% @doc Return dummy boxed item as html
-spec dummy_box(Panel_ID::list()) -> list().

dummy_box(Panel_ID) ->
   [spaces(6),"<div style=\"border: thin solid black\">\n",
    spaces(6),"<h2>SPOT</h2>\n <h3>panel: ", Panel_ID, "</h3>\n",
    "   </div>\n"].


%% @doc Return dummy image as html
-spec dummy_pic(Panel_ID::list()) -> list().

dummy_pic(Panel_ID) ->
   [spaces(6),"<div style=\"border: thin solid black\">\n",
    spaces(6),"<h2>SPOT</h2>\n <h3>", Panel_ID, "</h3>\n",
    spaces(6),"</div>\n"].

%% @doc Return dummy footer as html
-spec dummy_footer(Panel_ID::list()) -> list().

dummy_footer(Panel_ID) ->
   [spaces(6),"<div style=\"border-top: thin solid black\">\n",
    [spaces(6),"<h6>panel: ", Panel_ID, "</h6>\n"],
    spaces(6),"</div>\n"].


%% @doc Interpret dummy content
-spec content(Panel::tuple()) -> list().

content(Panel) ->
   {Panel_ID, _, _, Function, Parameters} = Panel,
   if
     Function == dummy -> [Type|_] = Parameters,
                          case Type of
                             "id"       -> dummy_id(Panel_ID);
                             "masthead" -> dummy_masthead(Panel_ID);
                             "nav"      -> dummy_nav(Panel_ID);
                             "squib"    -> dummy_squib(Panel_ID);
                             "clip"     -> dummy_clip(Panel_ID);
                             "article"  -> dummy_article(Panel_ID);
                             "box"      -> dummy_box(Panel_ID);
                             "pic"      -> dummy_pic(Panel_ID);
                             "footer"   -> dummy_footer(Panel_ID);
                             true       -> dummy_id(Panel_ID)
                           end;
     true               -> erlang:apply(zippity, Function, Parameters)
   end.






