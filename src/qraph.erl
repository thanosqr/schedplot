-module(qraph).
-compile(export_all).
-include("hijap.hrl").
-include_lib("wx/include/wx.hrl").

-define(STEP,5).

-define(ZOOM_IN,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}}).
-define(ZOOM_OUT,#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}).
-define(LEFT,#wx{event=#wxKey{keyCode=?WXK_LEFT}}).
-define(RIGHT,#wx{event=#wxKey{keyCode=?WXK_RIGHT}}).

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

loop(PDS)->
    receive
	{update,NPDS}->
	    loop(NPDS#pds{offset=PDS#pds.offset-?DETS_PACK_SIZE});
	WxEvent->
io:write(o),
	    NPDS=change_state(WxEvent,PDS),
	    loop(NPDS)
    end.


change_state(How,PDS)->
    case How of
	?LEFT->
	    io:write(l),
	    pds:move(PDS,-?STEP);
	?RIGHT->
	    pds:move(PDS,?STEP);
	_->
	    PDS
    end.

demo()->
    init(demo_trace).
init()->
    init(analyzed_trace).
init(Filename)->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),    


    {ok,Tab} = dets:open_file(Filename,[{access,read}]),
    [{init_state,Max_Zoom,CoreN}]=dets:lookup(Tab,init_state),
    wxFrame:show(Frame),  
timer:sleep(420),
    PDS=pds:new(Tab,1,CoreN,Max_Zoom,Max_Zoom,Frame),

io:write(lala),
    loop(PDS).


% new table format:
% {{CoreID, Zoom, Key},Start,Values}


cont(V)->
    lists:map(fun(_)->
			V     
	      end, lists:seq(1,1000)).
    
detgen()->
    {ok,T}=dets:open_file(demo_trace,[]),
    dets:insert(T,{init_state,5,1}),
    lists:map(fun(Y)->
		      lists:map(fun(X)->
					dets:insert(T,{{1,Y,X},42,cont(X+Y-1)})
				end,lists:seq(1,6))    
	      end,lists:seq(1,5)),
    dets:close(T).
    
wdemo()->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),


    Panel = wxPanel:new(Frame,[{size,{400,42}}]),
    wxFrame:show(Frame),
    timer:sleep(420),

    Paint=wxPaintDC:new(Panel),
    plotter:dl(Paint,42,42,420),
    wxPaintDC:destroy(Paint),
timer:sleep(2000),
    wxPanel:move(Panel,10,10).
    
test()->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),
    Panel=wxPanel:new(Frame,[{size,{1000,600}}]),
    wxFrame:show(Frame),
    timer:sleep(420),
    Paint = wxClientDC:new(Panel),
wxDC:clear(Paint),
    plotter:drawCoreLine(Paint,[[1,1,1,1,1,1,1,1,1,1,1]],42),
    wxClientDC:destroy(Paint),
timer:sleep(2442),
    wxPanel:move(Panel,100,0),
    Paint2 = wxClientDC:new(Panel),
    plotter:drawCoreLine(Paint2,[[11,11,11,11,11,11,11]],42),
    wxClientDC:destroy(Paint2).

    
