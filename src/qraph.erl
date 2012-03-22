-module(qraph).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down]).
-define(STEP,5).

-define(DETS_PACK_SIZE,1000).


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
	    NPDS=change_state(WxEvent,PDS),
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

demo()->
    init(demo_trace).
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
    Sizer=wxBoxSizer:new(?wxHORIZONTAL),
%    Panel2=wx:panel(Frame),
%    wxPanel:hide(Panel2),
    Panel=wxPanel:new(Frame),
    wxBoxSizer:add(Sizer,Panel),
    wxWindow:setSizer(Frame,Sizer),
wxFrame:show(Frame),    
timer:sleep(1000),

 %   PDS=pds:new(Tab,1,CoreN,Max_Zoom-2,Max_Zoom,Frame),

    Paint=wxBufferedPaintDC:new(Panel),
    plotter:dl(Paint,42,42,420),
    wxBufferedPaintDC:destroy(Paint),
   %% Paint2=wxBufferedPaintDC:new(Panel2),
   %%  plotter:dl(Paint,5,50,200),
   %%  wxBufferedPaintDC:destroy(Paint2),
%    wxFrame:show(Frame),    

    
io:write(lala),
    loop(aPDS).


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
					dets:insert(T,{{1,Y,X},42,cont(X)})
				end,lists:seq(1,6))    
	      end,lists:seq(1,5)),
    dets:close(T).
    
wdemo()->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),

    Ok = wxButton:new(Frame,?wxID_ANY,[{label,"OK"}]),
    Panel2 = wxPanel:new(Frame,[{size,{400,42}}]),
    wxPanel:hide(Panel2),
    Panel = wxPanel:new(Frame,[{size,{400,42}}]),
    timer:sleep(420),
    wxFrame:show(Frame),
    timer:sleep(420),
    Paint=wxPaintDC:new(Panel),
    plotter:dl(Paint,420,420,840),
    wxPaintDC:destroy(Paint),
    timer:sleep(420),
    Paint2=wxPaintDC:new(Panel2),
    plotter:dl(Paint2,420,420,840),
    wxPaintDC:destroy(Paint2),
    timer:sleep(2000),
    wxPanel:hide(Panel),
    
    wxPanel:show(Panel2).
%    wxPanel:hide(Panel),
    %% Sizer=wxBoxSizer:new(?wxHORIZONTAL),
    %% wxSizer:add(Sizer,Ok),
    %% wxSizer:add(Sizer,Panel).

    
