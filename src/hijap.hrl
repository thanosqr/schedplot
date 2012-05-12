-define(DETS_PACK_SIZE,2442).

-define(DEFAULT_FNAME,trace_famdict).
-define(DEFAULT_GNAME,trace_gabi).
-define(DEFAULT_TNAME,analyzed_trace).

-define(MAX_DUR,63).
-define(MAX_TIME,255).


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
				   zoomout_data=0
				  }).