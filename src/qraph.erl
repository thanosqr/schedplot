-module(qraph).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down]).
-define(STEP,5).
-define(PANEL_SIZE,6).
-define(DETS_PACK_SIZE,1000).


-define(ZOOM_IN,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}}).
-define(ZOOM_OUT,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}).
-define(LEFT,#wx{event=#wxKey{keyCode=?WXK_LEFT}}).
-define(RIGHT,#wx{event=#wxKey{keyCode=?WXK_RIGHT}}).

-record(panel,{Panel,
	       Key}).

loop(PDS)->
    receive
	{update,NPDS}->
	    loop(NPDS#pds{offset=PDS#pds.offset-?DETS_PACK_SIZE);
	WxEvent->
	    NPDS=change_state(WxEvent,State,Panels),
	    loop(NPDS)
    end.


change_state(How,PDS)->
    case How of
	?LEFT->
	    pds:move(PDS,-?STEP);
	?RIGHT->
	    pds:move(PDS,?STEP);
	_->
	    PDS
    end.


init()->
    init(analyzed_trace).
init(Filename)->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),
    
    
    lists:map(fun(X)->
		      wxEvtHandler:connect(Frame,X) %was panel
	      end, ?CONTROLS),

    {ok,Tab} = dets:open_file(Filename,[{access,read}]),
    [{init_state,Max_Zoom,CoreN}]=dets:lookup(Tab,init_state),
    Values=dets:match(Tab,{{'_',Max_Zoom-7,'_'},'$1'}),
    wxFrame:show(Frame),
timer:sleep(10),
    plotter:drawValues(Panel,Values),
    loop(Tab,PIDS,Panel).


% new table format:
% {{CoreID, Zoom, Key},Start,Values}


    
