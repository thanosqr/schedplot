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


												%when using buffdets zoom level refers to the index of the buffered zoom levels; ie if we buffer 5 zoom levels the possible values are 1..5 which can refer to n..n+5 actual IDs.

in()->
    init(analyzed_trace).

init()->
    init(demo_trace).

init(Filename)->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,?ANY,"",[{size,{1000,600}}]),  
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	wxMenu:append(File,?EXIT,"Quit"),
	wxMenuBar:append(MenuBar,File,"&File"),
	wxFrame:setMenuBar(Frame,MenuBar),
    wxFrame:show(Frame),  
    Panel=wxPanel:new(Frame,[{size,{1000,600}}]),
    lists:map(fun(XX)->
					  wxEvtHandler:connect(Panel,XX) 
			  end, ?CONTROLS),
	wxFrame:connect(Frame,command_menu_selected),
%	wxFrame:connect(Frame,close_window),
    Datapack=buffdets:open(Filename,Panel,Frame),
    NDatapack=draw(Datapack),
    loop(NDatapack).

draw(Datapack)->
    {Values,NDatapack} = buffdets:read(Datapack),
    Paint = wxBufferedPaintDC:new(Datapack#buffdets.panel),
    wxDC:clear(Paint),
    plotter:drawCoreLines(Paint,Values),
    wxBufferedPaintDC:destroy(Paint),
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
    case How of
		?RESIZE->
			same;
		?LEFT->
			if Datapack#buffdets.left_data>=?STEP ->
					Datapack#buffdets{pos={ZoomLvl,Xpos-?STEP},
								left_data=Datapack#buffdets.left_data-?STEP,
								right_data=Datapack#buffdets.right_data+?STEP};
			   true -> same
			end;
		?RIGHT->
			if Datapack#buffdets.right_data>=?STEP ->
					Datapack#buffdets{pos={ZoomLvl,Xpos+?STEP},
								left_data=Datapack#buffdets.left_data+?STEP,
								right_data=Datapack#buffdets.right_data-?STEP};
			   true -> same
			end;
		?ZOOM_IN->
			if Datapack#buffdets.zoomin_data>0 ->
					Datapack#buffdets{pos={ZoomLvl-1,Xpos},
								left_data=2*Datapack#buffdets.left_data,
								right_data=2*Datapack#buffdets.right_data,
								zoomin_data=Datapack#buffdets.zoomin_data-1,
								zoomout_data=Datapack#buffdets.zoomout_data+1};
			   true -> same
			end;
		?ZOOM_OUT->
			if Datapack#buffdets.zoomout_data>0 ->
					Datapack#buffdets{pos={ZoomLvl+1,Xpos},
								left_data=Datapack#buffdets.left_data div 2,
								right_data=Datapack#buffdets.right_data div 2,
								zoomin_data=Datapack#buffdets.zoomin_data+1,
								zoomout_data=Datapack#buffdets.zoomout_data-1};
			   true -> same
			end;

		_->
			same
    end.
