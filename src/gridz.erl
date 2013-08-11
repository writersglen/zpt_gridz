%%% ************************************************************
%%% @copyright 2013 Lloyd R. Prentice
%%% @author Lloyd R. Prentice lloyd@writersglen.com 
%%% @version 0.0.1
%%%
%%% @doc A page grid defines the graphic layout of an html page. Grids  
%%% are maintained in site records which provides metadata. Metadata 
%%% includes: create date and time, site_id, and width in Twitter 
%%% Bootstrap units (see: http://twitter.github.io/bootstrap/)
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
 

-module(gridz).

% grid helpers
-export([gen_id/0, get_created/1, get_site_id/1, put_site_id/2, get_width/1, get_page_layout/1]).

-export([new/1, new/2, clone/2, stack/3, partition/4, subdivide/3]). 
-export([update_panel_id/3, dummy_clip/3, merge/2, signature/1, dimensions/1, delete_row/2]).

-export([fluid/1, fixed/1, to_iolist/1, to_html/1]).
-export([test_grid/0, test_grid_as_iolist/0, test_grid_as_html/0]).

% -compile(export_all).

-include("../include/grid.hrl").


%% ************************************************************
%% gen_id/0 
%% ************************************************************


%% @doc Generate a unique ids
-spec gen_id() -> integer().

gen_id() ->
   erlang:phash2({node(), now()}).


%% ************************************************************
%% get_created/1 
%% ************************************************************


%% @doc given grid, return date and time created 
-spec get_created(Grid::tuple()) -> tuple().  

get_created(Grid) ->
   Grid#site.created.


%% ************************************************************
%% get_site_id/1 
%% ************************************************************


%% @doc Given grid, return site_id
-spec get_site_id(Grid::tuple()) -> integer().

get_site_id(Grid) ->
   Grid#site.site_id.


%% ************************************************************
%% put_site_id/2
%% ************************************************************


%% @doc Update site id 
-spec put_site_id(Grid::tuple(), New_ID::list) -> tuple().

put_site_id(Grid, New_ID) when is_list(New_ID) ->
   Grid#site{site_id=New_ID}.


%% ************************************************************
%% get_width/1 
%% ************************************************************


%% @doc given grid, return width
-spec get_width(Grid::tuple()) -> integer().

get_width(Grid) ->
   Grid#site.width.


%% ************************************************************
%% get_page_layout/1 
%% ************************************************************


%% @doc Return page layout
-spec get_page_layout(Grid::tuple()) -> list().

get_page_layout(Grid) ->
   Grid#site.page_layout.


%% ************************************************************
%% put_page_layout/2
%% ************************************************************


%% @doc given grid, return page layout
-spec put_page_layout(Grid::tuple(), Layout::tuple()) -> tuple().

put_page_layout(Grid, Layout) ->
   Grid#site{page_layout=Layout}.


%% ************************************************************
%% new/1 
%% ************************************************************


%% @doc Generate default grid  
-spec new(Page_ID::list()) -> tuple().

new(Page_ID) -> 
   Layout = rowz:stack(["top"]),
   #site{site_id="grid_lib", page_id=Page_ID, page_layout=Layout}.
   

%% ************************************************************
%% new/2 
%% ************************************************************


%% @doc Generate default grid; one row, N panels where N = length
%% Panel_IDs 
-spec new(Page_ID::list(), Panel_IDs::list()) -> tuple().

new(Page_ID, Panel_IDs) -> 
   Layout = rowz:stack(Panel_IDs),
   #site{site_id="grid_lib", page_id=Page_ID, page_layout=Layout}.
   

%% ************************************************************
%% clone/2 
%% ************************************************************


%% @doc Clone a grid with a new page_id 
-spec clone(Grid::tuple(), Page_ID::list()) -> tuple().

clone(Grid, Page_ID) ->
   Grid#site{page_id=Page_ID}.


%% ************************************************************
%% stack/3 
%% ************************************************************


%% @doc Stack one or more  new panels onto an existing panel in grid 
-spec stack(Grid::tuple(), Panel_ID::list(), Names::list()) -> tuple().

stack(Grid, Panel_ID, Names) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:stack(Layout, Panel_ID, Names),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% partition/1 
%% ************************************************************


%% @doc Partition a panel in grid into two columns; first column has span of N 
-spec partition(Grid::tuple(), Panel_ID::list(), N::integer(), Name::list()) -> tuple().

partition(Grid, ID, N, Name) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:partition(Layout, ID, N, Name),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% subdivide/3 
%% ************************************************************


%% @doc Subdivide a column
-spec subdivide(Grid::tuple(), Confirm_ID::list(), Names::list()) -> tuple().

subdivide(Grid, Confirm_ID, Names) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:subdivide(Layout, Confirm_ID, Names),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% update_panel_id/3 
%% ************************************************************


%% @doc Update a panel ID
-spec update_panel_id(Grid::tuple(), Panel_ID::list(), New_ID::list()) -> tuple().

update_panel_id(Grid, Panel_ID, New_ID) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:update_panel_id(Layout, Panel_ID, New_ID),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% dummy_clip/3 
%% ************************************************************


%% @doc Update dummy content; valid tokens: "id", "masthead", "nav", "squib", 
%%      "clip", "article", "box", "pic", "footer" 
-spec dummy_clip(Grid::tuple(), Panel_ID::list(), Token::list()) -> tuple().

dummy_clip(Grid, Panel_ID, Token) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:dummy_clip(Layout, Panel_ID, Token),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% merge/1 
%% ************************************************************


%% @doc Merge adjacent panels
-spec merge(Row::tuple(), Confirm_ID::list) -> tuple().

merge(Grid, Confirm_ID) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:merge(Layout, Confirm_ID),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% signature/1 
%% ************************************************************


%% @doc Return signature of grid; e.g. number of rows and panels 
-spec signature(Grid::tuple()) -> list().

signature(Grid) ->
   rowz:signature(get_page_layout(Grid)).


%% ************************************************************
%% dimensions/1 
%% ************************************************************


%% @doc Report number of rows and panels 
-spec dimensions(Grid::tuple()) -> list().

dimensions(Grid) ->
   rowz:dimensions(get_page_layout(Grid)).


%% ************************************************************
%% delete_row/2 
%% ************************************************************


%% @doc Delete row 
-spec delete_row(Grid::tuple(), Confirm_ID::list()) -> tuple().

delete_row(Grid, Confirm_ID) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:delete_row(Layout, Confirm_ID),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% fluid/1 
%% ************************************************************


%% @doc Configure fluid grid (See Twitter Bootstrap for details)
-spec fluid(Row::tuple()) -> tuple().

fluid(Grid) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:fluid(Layout),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% fixed/1 
%% ************************************************************


%% @doc Configure fixed grid (See Twitter Bootstrap for details)
-spec fixed(Row::tuple()) -> tuple().

fixed(Grid) ->
   Layout = get_page_layout(Grid),
   Layout1 = rowz:fixed(Layout),
   put_page_layout(Grid, Layout1).


%% ************************************************************
%% to_iolist/1 
%% ************************************************************


%% @doc Render grid as I/O list 
-spec to_iolist(Grid::tuple()) -> list().

to_iolist(Grid) ->
   Layout = get_page_layout(Grid),
   rowz:to_iolist(Layout).


%% ************************************************************
%% to_html/1  
%% ************************************************************


%% @doc Render grid to html
-spec to_html(Grid::tuple()) -> list().

to_html(Grid) ->
   Layout = get_page_layout(Grid),
   rowz:to_html(Layout).


%% ************************************************************
%% test_grid/1 
%% ************************************************************


%% @doc Generate test grid 
-spec test_grid() -> list().

test_grid() ->
   G  = gridz:new("my_grid", ["logo", "nav", "left_sidebar", "footer"]),
   gridz:partition(G, "left_side_bar", 4, "content").


%% ************************************************************
%% test_grid_as_iolist/1 
%% ************************************************************


%% @doc Generate test grid as iolist
-spec test_grid_as_iolist() -> iolist().

test_grid_as_iolist() ->
   G = test_grid(),
   to_iolist(G).


%% ************************************************************
%% test_grid_as_html/1 
%% ************************************************************


%% @doc Generate test grid as html 
-spec test_grid_as_html() -> list().

test_grid_as_html() ->
   G = test_grid(),
   to_html(G).


 



