-module(uds).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
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
-define(ANY,?wxID_ANY).
-define(ZOOM_IN,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}}).
-define(ZOOM_OUT,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}).
-define(LEFT,#wx{event=#wxKey{keyCode=?WXK_LEFT}}).
-define(RIGHT,#wx{event=#wxKey{keyCode=?WXK_RIGHT}}).
-define(RESIZE,#wx{event=wxEVT_SIZE}).
-define(QUIT,#wx{id=?EXIT,event=#wxCommand{type=command_menu_selected}}).


-record(panel,{width=1000,
			   panel,
			   frame,
			   xpos=1,
			   zoomlvl=1,
			   left_data=0,
			   right_data,
			   zoomin_data,
			   zoomout_data=0}).

												%when using buffdets zoom level refers to the index of the buffered zoom levels; ie if we buffer 5 zoom levels the possible values are 1..5 which can refer to n..n+5 actual IDs.

in()->
    init(analyzed_trace).

init()->
    init(demo_trace).

init(Filename)->
    {Zs,Xs,Datapack}=buffdets:open(Filename),
	io:write({Zs,Xs}),
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,?ANY,"",[{size,{1000,600}}]),  
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	wxMenu:append(File,?EXIT,"Quit"),
	wxMenuBar:append(MenuBar,File,"&File"),
	wxFrame:setMenuBar(Frame,MenuBar),
    wxFrame:show(Frame),  
    Panel=#panel{panel=wxPanel:new(Frame,[{size,{1000,600}}]),
				 frame=Frame,
				 right_data=Xs,
				 zoomin_data=Zs},
    lists:map(fun(X)->
					  wxEvtHandler:connect(Panel#panel.panel,X) 
			  end, ?CONTROLS),
	wxFrame:connect(Frame,command_menu_selected),
%	wxFrame:connect(Frame,close_window),
    draw(Panel,Datapack),
    loop(Panel,Datapack),
	wx:destroy().


draw(Panel, Datapack)->
    Values = buffdets:getData(Panel#panel.xpos,Panel#panel.width,Panel#panel.zoomlvl,Datapack),
    io:write(length(lists:nth(1,Values))),io:nl(),
    Paint = wxBufferedPaintDC:new(Panel#panel.panel),
    wxDC:clear(Paint),
    plotter:drawCoreLines(Paint,Values),
    wxBufferedPaintDC:destroy(Paint).


loop(Panel,Datapack)->
	receive
		?QUIT->
			wxWindow:close(Panel#panel.frame,[]);
		WxEvent->
			NPanel=change_state(WxEvent,Panel),
			draw(NPanel,Datapack),
			loop(NPanel,Datapack)
    end.

change_state(How,Panel)->
    case How of
		?RESIZE->
		 	io:write(yuuu),
			Panel;
		?LEFT->
			if Panel#panel.left_data>=?STEP ->
					Panel#panel{xpos=Panel#panel.xpos-?STEP,
								left_data=Panel#panel.left_data-?STEP,
								right_data=Panel#panel.right_data+?STEP};
			   true -> Panel
			end;
		?RIGHT->
			if Panel#panel.right_data>=?STEP ->
					Panel#panel{xpos=Panel#panel.xpos+?STEP,
								left_data=Panel#panel.left_data+?STEP,
								right_data=Panel#panel.right_data-?STEP};
			   true -> Panel
			end;
		?ZOOM_IN->
			if Panel#panel.zoomin_data>0 ->
					Panel#panel{zoomlvl=Panel#panel.zoomlvl+1,
								xpos=2*Panel#panel.xpos-1,
								left_data=2*Panel#panel.left_data,
								right_data=2*Panel#panel.right_data,
								zoomin_data=Panel#panel.zoomin_data-1,
								zoomout_data=Panel#panel.zoomout_data+1};
			   true -> Panel
			end;
		?ZOOM_OUT->
			if Panel#panel.zoomout_data>0 ->
					Panel#panel{zoomlvl=Panel#panel.zoomlvl-1,
								xpos=(Panel#panel.xpos+1) div 2,
								left_data=Panel#panel.left_data div 2,
								right_data=Panel#panel.right_data div 2,
								zoomin_data=Panel#panel.zoomin_data+1,
								zoomout_data=Panel#panel.zoomout_data-1};
			   true -> Panel
			end;

		_->
			Panel
    end.
