Erlang-based HTML Page Grid Editor

A page grid defines the abstract graphic layout of a printed page or visual display; e.g. columns, boxes, and other two-dimensional containers for the various text and graphic elements that make up a well-designed page. Page grids provide structural organization of content and help establish an effective "eye path" for purusing the page. The page grid also helps establish a distinct and consistent visual identity across multiple pages and instances of a publication or website.

zpt_gridz is based on a Twitter Bootstrap 12-column grid. It is still highly experimental. As stands it provides two techniques for creating page-grids: 1) A set of Erlang functions that can be mixed and matched to output a page grid as either an Erlang iolist or as html; 2) A page-grid server and API. As stands, the programmable functions may not be optimal for maximum productivity. So one goal of this version is to test and revise the grid creation functions toward a more efficient and comprehensive set.

Experienced web developers may prefer to program custom grids in Erlang source. As noted below, this can be very efficient. Less experienced developers may prefer to use GUI tools (yet to be developed) to select pre-designed grids from the grid archive and tweak them in a GUI-based grid_editing tool.

The main goal of this release is to test and revise the current set of grid generation functions toward optimal functionality and code improvement. The author is learning Erlang as he goes, so he welcomes all input that will help improve functionality and clarity.
Data Structures

Panel records define html DIV structures, including id, class, and content. They also include Twitter width (span) and offset parameters. Content is defined through mfa "content brokers." NOTE: The content broker concept is provisional. There may be more optimal ways of representing data. A set of "dummy" content brokers is included in the clipz source file.

   -record(panel, {id= "top", class="span", span=?MAXSPAN, offset=0, module=formatz, function=dummy, parameters=[id]}).

Row records define Twitter Bootstrap rows. The span parameter is for convenience in traversing grid structures.

   -record(row, {class="row", span=12, panels=[]}).

Page grids are essentially recursive rows. Since every html page will have a unique layout defined by the page grid. Thus, page grids are stored and managed in site records uniquely identified by site_id and page_id. Archived page grids are stored under the "grid_lib" site_id.

   -record(site, {site_id, created=now(),  page_id, width=?MAXSPAN, page_layout}).

Example: How to Generate a Page Grid From Erlang Source

Source:

test_grid() ->
   G  = gridz:new("my_grid", ["hed", "dek", "nav","left_sidebar","footer"]),
   gridz:partition(G, "left_sidebar", 4, "content").

Generation:

   1> G = gridz:test_grid().

   2> gridz:to_html(G).
