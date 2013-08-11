

-define(BOOTSTRAP_CLASS, "span").
-define(MAXSPAN, 12).
-define(INDENT, 5).


-record(site, {site_id, created=now(),  page_id, width=?MAXSPAN, page_layout}).

-record(row, {class="row", span=12, panels=[]}).

-record(panel, {panel_id="top", class="span", span=?MAXSPAN, offset=0, module=clipz, function=dummy, parameters=[id]}).

