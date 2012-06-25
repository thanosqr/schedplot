-module(uds).
-compile(export_all).

-include("hijap.hrl").

-define(HEIGHT_INT,42). % graph height + space between cores in px
-define(ZOOM_BUFFER,2).
-define(UNIT_WIDTH,42).
-define(PANEL_SIZE,6).
-define(MAX_OFFSET,1000).
-define(MIN_OFFSET,-1000).
-define(STEP_ALT,10).
-define(STEP_NORM,50).
-define(STEP_CMD,200).
-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down,size]).

-define(EXIT,?wxID_EXIT).

-define(QUIT,#wx{id=?EXIT,event=#wxCommand{type=command_menu_selected}}).


%%when using buffdets zoom level refers to the index of the buffered zoom levels; ie if we buffer 5 zoom levels the possible values are 1..5 which can refer to n..n+5 actual IDs.


init(FolderName)->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,?ANY,"",[{size,{?WIDTH,?HEIGHT}}]),  
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File,?EXIT,"Quit"),
    wxMenuBar:append(MenuBar,File,"&File"),
    wxFrame:setMenuBar(Frame,MenuBar),
    wxFrame:show(Frame),  
    Panel=create_panel(Frame,?PWIDTH,?PHEIGHT),
    wxFrame:connect(Frame,command_menu_selected),
    wxFrame:connect(Frame,size),
    %%wxFrame:connect(Frame,close_window),
    Datapack=buffdets:open(FolderName,Panel,Frame),
    NDatapack=draw(Datapack),
    loop(NDatapack).

create_panel(Frame,W,H)->
    Panel=wxPanel:new(Frame,[{size,{W,H}},{pos,{42,0}}]),
    lists:map(fun(XX)->
		      wxEvtHandler:connect(Panel,XX) 
	      end, ?CONTROLS),
    Panel.

draw(Datapack)->
    Paint = wxBufferedPaintDC:new(Datapack#buffdets.panel),    
    wxDC:clear(Paint),
    NDatapack=draw(Datapack,Paint),
    wxBufferedPaintDC:destroy(Paint),
    update_zoom_label(NDatapack#buffdets.static,
		      NDatapack#buffdets.pos,
		      NDatapack#buffdets.offset,
		      NDatapack#buffdets.zoom_label,
		      NDatapack#buffdets.width,
		      NDatapack#buffdets.height),
    NDatapack.

draw(Datapack,Paint)->
    {Values,NDatapack} = buffdets:read(Datapack),
    plotter:drawGrid(Paint,NDatapack#buffdets.offset,
		     NDatapack#buffdets.pos,
		     NDatapack#buffdets.labels,
		     NDatapack#buffdets.height+?PH_DIFF,
		     NDatapack#buffdets.width+?PW_DIFF),
    plotter:drawCoreLines(Paint,Values,
			  lists:nthtail(NDatapack#buffdets.fromCore,
					NDatapack#buffdets.schedlabels),
			  NDatapack#buffdets.width+?PW_DIFF,
			  NDatapack#buffdets.height+?PH_DIFF,
			  NDatapack#buffdets.vzoom),
    scarlet:draw(Paint,
		 NDatapack#buffdets.fromCore,
		 NDatapack#buffdets.pos,
		 NDatapack#buffdets.offset,
		 NDatapack#buffdets.width,
		 NDatapack#buffdets.scarlet,
		 NDatapack#buffdets.vzoom),
    NDatapack.
    

loop(Datapack)->
    receive
	?QUIT->
	    wxWindow:close(Datapack#buffdets.frame,[]),
	    wx:destroy();
	WxEvent when erlang:is_record(WxEvent,wx)->
	    case change_state(WxEvent,Datapack) of
		same->
		    NNDatapack=Datapack;
		NDatapack->
		    NNDatapack=draw(NDatapack)
	    end,
	    case NNDatapack#buffdets.mode of
		ready->
		    loop(NNDatapack);
		update ->
		    receive
			{new_buffer,N3Datapack}->
			    loop(N3Datapack)
		    end
	    end
    end.

change_state(How,Datapack)->
    {ZoomLvl,Xpos} = Datapack#buffdets.pos,
    {_,Xoff} = Datapack#buffdets.offset,
    case change_decode(How) of
	{resize,W,H}->
	    wxPanel:destroy(Datapack#buffdets.panel),
	    Datapack#buffdets{width=W,
			      height=H,
			      panel=create_panel(Datapack#buffdets.frame,
						 W+?PW_DIFF,H+?PH_DIFF)};
	{left,Step}->
	    if Datapack#buffdets.left_data>=Step ->
		    Datapack#buffdets{pos={ZoomLvl,Xpos-Step},
				      left_data=Datapack#buffdets.left_data-Step,
				      right_data=Datapack#buffdets.right_data+Step};
	       true -> same
	    end;
	{right,Step}->
	    if Datapack#buffdets.right_data>=Step ->
		    Datapack#buffdets{pos={ZoomLvl,Xpos+Step},
				      left_data=Datapack#buffdets.left_data+Step,
				      right_data=Datapack#buffdets.right_data-Step};
	       true -> same
	    end;
	zoom_in->
	    if Datapack#buffdets.zoomin_data>0 ->
		    Datapack#buffdets{pos={ZoomLvl-1,(Xpos-?DETS_PACK_SIZE+Xoff)*2+?DETS_PACK_SIZE-Xoff},
				      left_data=2*Datapack#buffdets.left_data,
				      right_data=2*Datapack#buffdets.right_data,
				      zoomin_data=Datapack#buffdets.zoomin_data-1,
				      zoomout_data=Datapack#buffdets.zoomout_data+1};
	       true -> same
	    end;
	zoom_out->
	    if Datapack#buffdets.zoomout_data>0 ->
		    Datapack#buffdets{pos={ZoomLvl+1,((Xpos-?DETS_PACK_SIZE+Xoff) div 2)+?DETS_PACK_SIZE-Xoff},
				      left_data=Datapack#buffdets.left_data div 2,
				      right_data=Datapack#buffdets.right_data div 2,
				      zoomin_data=Datapack#buffdets.zoomin_data+1,
				      zoomout_data=Datapack#buffdets.zoomout_data-1};
	       true -> same
	    end;
	reset->
	    {BufferXsize,BufferZsize,CoreN}=Datapack#buffdets.static,
	    buffdets:create_buffer(Datapack#buffdets.tab,
				   BufferXsize,BufferZsize,CoreN,
				   Datapack#buffdets.panel,
				   Datapack#buffdets.frame,
				   Datapack#buffdets.max_zoom,
				   Datapack#buffdets.labels,
				   Datapack#buffdets.width,
				   Datapack#buffdets.zoom_label,
				   Datapack#buffdets.schedlabels,
				   Datapack#buffdets.scarlet
				  );
	{core_up,CN}->
	    FromCore = Datapack#buffdets.fromCore,
	    if FromCore-CN>=0 ->
		    hide_extra_sched_labels(FromCore-CN,Datapack#buffdets.schedlabels),
		    Datapack#buffdets{fromCore=FromCore-CN};
	       true->
		    same
	    end;
	{core_down,CN}->
	    FromCore = Datapack#buffdets.fromCore,
	    {_,_,CoreN} = Datapack#buffdets.static,
	    if FromCore+CN<CoreN->
		    hide_extra_sched_labels(FromCore+CN,Datapack#buffdets.schedlabels),
		    Datapack#buffdets{fromCore=FromCore+CN};
	       true->
		    same
	    end;
	print->
	    %% {_,_,T}=erlang:now(),
	    %% N=lists:concat([atom_to_list(print),integer_to_list(T),atom_to_list('.bmp')]),
	    %% B = wxBitmap:new(Datapack#buffdets.width,
	    %% 		    ?PHEIGHT),
	    %% M = wxMemoryDC:new(),
	    %% wxMemoryDC:selectObject(M,B),
	    %% P = wxPaintDC:new(Datapack#buffdets.panel),
	    %% draw(Datapack,P),
	    %% wxDC:setLogicalFunction(P,?wxCOPY),
	    %% wxDC:setLogicalFunction(M,?wxCOPY),
	    %% wxDC:blit(M,{0,0},{Datapack#buffdets.width,?PHEIGHT},P,{0,0}),
	    %% wxBitmap:saveFile(B,N,?wxBITMAP_TYPE_GIF),
	    %% wxMemoryDC:destroy(M),
	    %% wxBitmap:destroy(B),
	    %% io:format("screenshot saved\n"),
	    same;
	same->
	    same
    end.


update_zoom_label({_,_,CoreN},{Z,_},{ZOffset,_},Label,W,H)->
    Zm=round(math:pow(2,Z+ZOffset+?DEF_GU-1)),
    wxWindow:move(Label,W+?ZLW,H+?ZLH),
    wxStaticText:setLabel(Label,
	  lists:concat(["Zoom 1:", 
			integer_to_list(Zm),
			"  [1px = ",
			plotter:label_portray(Zm),
			"]",
			"  Cores: ",
			integer_to_list(CoreN)])).

hide_extra_sched_labels(0,_)->ok;
hide_extra_sched_labels(FromCore,[L|Ls])->
    wxWindow:move(L,-42,-42),
    hide_extra_sched_labels(FromCore-1,Ls).

change_decode(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}})->
    zoom_in;
change_decode(#wx{event=#wxKey{keyCode=61}}) ->
    zoom_in;
change_decode(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}) ->
    zoom_out;
change_decode(#wx{event=#wxKey{keyCode=45}}) ->
    zoom_out;
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT,altDown=true}}) ->
    {left,?STEP_ALT};
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT,controlDown=true}}) ->
    {left,?STEP_CMD};
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT}}) ->
    {left,?STEP_NORM};
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT,altDown=true}}) ->
    {right,?STEP_ALT};
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT,controlDown=true}}) ->
    {right,?STEP_CMD};
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT}}) ->
    {right,?STEP_NORM};
change_decode(#wx{event=#wxKey{keyCode=?WXK_HOME}}) ->
    reset;
change_decode(#wx{event=#wxKey{keyCode=?WXK_UP,altDown=true}}) ->
    {core_up,10};
change_decode(#wx{event=#wxKey{keyCode=?WXK_DOWN,altDown=true}}) ->
    {core_down,10};
change_decode(#wx{event=#wxKey{keyCode=?WXK_UP,altDown=false}}) ->
    {core_up,1};
change_decode(#wx{event=#wxKey{keyCode=?WXK_DOWN,altDown=false}}) ->
    {core_down,1};
change_decode(#wx{event=#wxSize{size={W,H}}}) ->
    {resize,W,H};
change_decode(#wx{event=#wxKey{keyCode=80}})->
    print;
change_decode(_) ->
    same.
