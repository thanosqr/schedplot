
-include_lib("wx/include/wx.hrl").
-define(DETS_PACK_SIZE,4096).

-define(DEFAULT_FNAME,trace_famdict).
-define(DEFAULT_GNAME,trace_gabi).
-define(DEFAULT_TNAME,analyzed_trace).
-define(DEFAULT_FOLDER_NAME,qijap_profile).

-define(VERTICAL_INT,100).
-define(MAX_DUR,63).
-define(MAX_TIME,255).
-define(ANY,?wxID_ANY).

-define(DEF_GU,3).
-define(GU,8).

-define(HEIGHT,700).
-define(PHEIGHT,?HEIGHT-100).
-define(PWIDTH,1000).
-define(WIDTH,?PWIDTH+84).

% GU = math:pow(2,DEF_GU) but used in guards
-record(buffdets, {data,
		   tab,
		   pos,
		   offset,
		   mode=ready,
		   static,
		   ux,
		   lx,
		   uz,
		   lz,
		   width=1000,
		   panel,
		   frame,
		   left_data=0,
		   right_data,
		   zoomin_data,
		   zoomout_data=0,
		   labels=[],
		   zoom_label,
		   max_zoom,
		   schedlabels,
		   scarlet
		  }).
						%zoom level: 0 = 1-1, max_zoom = reduced to 1 value
