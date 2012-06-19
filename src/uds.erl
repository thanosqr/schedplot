-module(uds).
-compile(export_all).

-include("hijap.hrl").

-define(HEIGHT_INT,42). % graph height + space between cores in px
-define(ZOOM_BUFFER,2).
-define(UNIT_WIDTH,42).
-define(PANEL_SIZE,6).
-define(MAX_OFFSET,1000).
-define(MIN_OFFSET,-1000).
-define(STEP,5).
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

    Panel=wxPanel:new(Frame,[{size,{?PWIDTH,?PHEIGHT}},{pos,{42,0}}]),
    lists:map(fun(XX)->
		      wxEvtHandler:connect(Panel,XX) 
	      end, ?CONTROLS),
    wxFrame:connect(Frame,command_menu_selected),
    %%wxFrame:connect(Frame,close_window),
    Datapack=buffdets:open(FolderName,Panel,Frame),
    NDatapack=draw(Datapack),
    loop(NDatapack).

draw(Datapack)->
    {Values,NDatapack} = buffdets:read(Datapack),

    Paint = wxBufferedPaintDC:new(Datapack#buffdets.panel),
    wxDC:clear(Paint),
    plotter:drawGrid(Paint,NDatapack#buffdets.offset,
		     NDatapack#buffdets.pos,
		     NDatapack#buffdets.labels),
    plotter:drawCoreLines(Paint,Values,NDatapack#buffdets.schedlabels),
    scarlet:draw(Paint,
		 NDatapack#buffdets.pos,
		 NDatapack#buffdets.offset,
		 NDatapack#buffdets.width,
		 NDatapack#buffdets.scarlet),
    wxBufferedPaintDC:destroy(Paint),
    update_zoom_label(NDatapack#buffdets.pos,
		      NDatapack#buffdets.offset,
		      NDatapack#buffdets.zoom_label),
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
    case change_decode(How) of
	resize->
	    same;
	left->
	    if Datapack#buffdets.left_data>=?STEP ->
		    Datapack#buffdets{pos={ZoomLvl,Xpos-?STEP},
				      left_data=Datapack#buffdets.left_data-?STEP,
				      right_data=Datapack#buffdets.right_data+?STEP};
	       true -> same
	    end;
	right->
	    if Datapack#buffdets.right_data>=?STEP ->
		    Datapack#buffdets{pos={ZoomLvl,Xpos+?STEP},
				      left_data=Datapack#buffdets.left_data+?STEP,
				      right_data=Datapack#buffdets.right_data-?STEP};
	       true -> same
	    end;
	zoom_in->
	    if Datapack#buffdets.zoomin_data>0 ->
		    Datapack#buffdets{pos={ZoomLvl-1,Xpos},
				      left_data=2*Datapack#buffdets.left_data,
				      right_data=2*Datapack#buffdets.right_data,
				      zoomin_data=Datapack#buffdets.zoomin_data-1,
				      zoomout_data=Datapack#buffdets.zoomout_data+1};
	       true -> same
	    end;
	zoom_out->
	    if Datapack#buffdets.zoomout_data>0 ->
		    Datapack#buffdets{pos={ZoomLvl+1,Xpos},
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
	same->
	    same
    end.


update_zoom_label({Z,_},{ZOffset,_},Label)->
    wxStaticText:setLabel(Label,
			  lists:concat(["Zoom 1:", 
					integer_to_list(round(math:pow(2,Z+ZOffset+?DEF_GU-1)))])).


change_decode(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}})->
    zoom_in;
change_decode(#wx{event=#wxKey{keyCode=?WXK_DOWN}}) ->
    zoom_in;
change_decode(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}) ->
    zoom_out;
change_decode(#wx{event=#wxKey{keyCode=?WXK_UP}}) ->
    zoom_out;
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT}}) ->
    left;
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT}}) ->
    right;
change_decode(#wx{event=#wxKey{keyCode=?WXK_HOME}}) ->
    reset;
change_decode(#wx{event=wxEVT_SIZE}) ->
    resize;
change_decode(_) ->
    same.
