-module(uds).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

-define(CORE_LIMIT,1000).
-define(HEIGHT_INT,42). % graph height + space between cores in px
-define(ZOOM_BUFFER,2).
-define(UNIT_WIDTH,42).
-define(DETS_PACK_SIZE,1000).
-define(PANEL_SIZE,6).
-define(MAX_OFFSET,1000).
-define(MIN_OFFSET,-1000).
-define(STEP,5).
-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down]).


-define(ZOOM_IN,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}}).
-define(ZOOM_OUT,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}).
-define(LEFT,#wx{event=#wxKey{keyCode=?WXK_LEFT}}).
-define(RIGHT,#wx{event=#wxKey{keyCode=?WXK_RIGHT}}).

-record(panel,{width=1000,
	       panel,
	       xpos=1,
	       zoomlvl=1}).

%when using buffdets zoom level refers to the index of the buffered zoom levels; ie if we buffer 5 zoom levels the possible values are 1..5 which can refer to n..n+5 actual IDs.

init()->
    init(demo_trace).

init(Filename)->
    Datapack=buffdets:open(Filename),
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),  
    wxFrame:show(Frame),  
    Panel=#panel{panel=wxPanel:new(Frame,[{size,{1000,600}}])},
    lists:map(fun(X)->
		      wxEvtHandler:connect(Panel#panel.panel,X) 
	      end, ?CONTROLS),
    draw(Panel,Datapack),
    loop(Panel,Datapack).


draw(Panel, Datapack)->
    Values = buffdets:getData(Panel#panel.xpos,Panel#panel.width,Panel#panel.zoomlvl,Datapack),
    Paint = wxBufferedPaintDC:new(Panel#panel.panel),
    wxDC:clear(Paint),
    plotter:drawCoreLines(Paint,Values),
    wxBufferedPaintDC:destroy(Paint).


loop(Panel,Datapack)->
        receive
%	{update,NPDS}->
%	    loop(NPDS#pds{offset=PDS#pds.offset-?DETS_PACK_SIZE});
	WxEvent->
		io:write(o),
		NPanel=change_state(WxEvent,Panel),
		draw(NPanel,Datapack),
		loop(NPanel,Datapack)
    end.

change_state(How,Panel)->
    case How of
	?LEFT->
	    io:write(l),
	    Panel#panel{xpos=Panel#panel.xpos-?STEP};
	?RIGHT->
	    Panel#panel{xpos=Panel#panel.xpos+?STEP};
	?ZOOM_IN->
	    io:write(in),
	    Panel#panel{zoomlvl=Panel#panel.zoomlvl+1};
	?ZOOM_OUT->
	    Panel#panel{zoomlvl=Panel#panel.zoomlvl-1};
	_->
	    Panel
    end.
