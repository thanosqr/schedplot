-module(pds).
-compile(export_all).
-define(CORE_LIMIT,1000).
-define(HEIGHT_INT,42). % graph height + space between cores in px
-define(ZOOM_BUFFER,2).
-define(UNIT_WIDTH,42).
-define(DETS_PACK_SIZE,1000).
-define(PANEL_SIZE,6).
-define(MAX_OFFSET,1000).
-define(MIN_OFFSET,-1000).

-record(pds,{tab,
	     current,
	     in,
	     out,
	     first_key,
	     max_cores,
	     zoom,
	     max_zoom,
	     frame,
	     offset}).

panelCreate(Zoom,KeyStart,Table,MaxCore,Frame)->
    Panel=wxPanel:new(Frame),
    wxPanel:hide(Panel),
    Paint = wxBufferedPaintDC:new(Panel),
    lists:map(fun(CoreID)->
		      case getValues(CoreID,Zoom,KeyStart,Table) of
			  []->io:write(uh),ok;
			  Values->
			      plotter:drawCoreLine(Paint,[Values],CoreID*42)
		      end
	      end, lists:seq(1,MaxCore)),
    wxBufferedPaintDC:destroy(Paint),
    Panel.


getValues(CoreID,Zoom,KeyStart,Table)->
    lists:flatten(lists:map(fun(Key)->
		     dets:match(Table,{{CoreID,Zoom,Key},'_','$1'})	      
		   end, lists:seq(KeyStart,KeyStart+?PANEL_SIZE-1))).

new(Table,FirstKey,Max_Cores,Zoom,Max_Zoom,Frame)->
    Current=panelCreate(Zoom,FirstKey,Table,Max_Cores,Frame),
    io:write(wxPanel:show(Current)),
    In = lists:map(fun(ZoomP)->
			   panelCreate(ZoomP,FirstKey,Table,Max_Cores,Frame)
		   end,lists:seq(Zoom-?ZOOM_BUFFER,Zoom-1)),
    Out = lists:map(fun(ZoomP)->
			   panelCreate(ZoomP,FirstKey,Table,Max_Cores,Frame)
		    end,lists:seq(Zoom+1,Zoom+?ZOOM_BUFFER)),
    #pds{tab=Table,
	 current=Current,
	 in=In,
	 out=Out,
	 first_key=FirstKey,
	 max_cores=Max_Cores,
	 zoom=Zoom,
	 max_zoom=Max_Zoom,
	 frame=Frame,
	 offset=0
	}.
    
move(PDS,Offset)->
    spawn(?MODULE,check_offset,[Offset,PDS,wx:get_env(),self()]),               
%we assume that we will update before we reach the hard limit
%however, maybe a strick check should be introduced
    wxPanel:move(PDS#pds.current,Offset,0),
    PDS#pds{offset=PDS#pds.offset+Offset}.

check_offset(Offset,PDS,Env,PID)->
    wx:set_env(Env),
    case PDS#pds.offset+Offset of
	?MAX_OFFSET->
	    PID!{update,new(PDS#pds.tab,
			    PDS#pds.first_key+1,
			    PDS#pds.max_cores,
			    PDS#pds.zoom,
			    PDS#pds.max_zoom,
			    PDS#pds.frame)};
	?MIN_OFFSET ->
	    PID!{update,new(PDS#pds.tab,
			    PDS#pds.first_key-1,
			    PDS#pds.max_cores,
			    PDS#pds.zoom,
			    PDS#pds.max_zoom,
			    PDS#pds.frame)};
	_ ->
	    ok
    end.
