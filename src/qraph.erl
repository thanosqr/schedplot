-module(qraph).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down]).

loop(State,PIDS,Panel)->
    receive
	WxEvent->
	    case decode_bindings(WxEvent) of
		null ->
		    loop(State,PIDS,Panel);
		DB ->
%		    plotter:clear_canvas(Panel),
		    Paint = wxBufferedPaintDC:new(Panel),

		    NState=change_state(DB,State,PIDS,Paint),

		    wxBufferedPaintDC:destroy(Paint),

		    loop(NState,PIDS,Panel)
	    end
    end.

decode_bindings(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}})->
    zoom_in;
decode_bindings(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}})->
    zoom_out;
decode_bindings(#wx{event=#wxKey{keyCode=?WXK_LEFT}}) ->
    left;
decode_bindings(#wx{event=#wxKey{keyCode=?WXK_RIGHT}}) ->
    right;
decode_bindings(_)->
    null.

change_state(How,State,PIDS,Paint)->
    wxDC:clear(Paint),
    lists:map(fun(X)->
		      X!{How,Paint,self()}
	      end,PIDS),

    lists:map(fun(_X)->
		      receive
			  _ -> ok
		      end
	      end,PIDS),



    State.

init()->
    init(analyzed_trace).
init(Filename)->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{1000,600}}]),
    Panel=wxPanel:new(Frame),
    lists:map(fun(X)->
		      wxEvtHandler:connect(Panel,X) 
	      end, ?CONTROLS),
    {ok,Tab} = dets:open_file(Filename,[{access,read}]),
    [{init_state,Max_Zoom,CoreN}]=dets:lookup(Tab,init_state),
    Env = wx:get_env(),
    PIDS=lists:map(fun(X)->
			   spawn(qraph,core_init,
				 [Tab,X,Panel,Max_Zoom,1000,Env])  
		   end,
		   lists:seq(1,CoreN)),
    Values=dets:match(Tab,{{'_',Max_Zoom-7,'_'},'$1'}),
    wxFrame:show(Frame),
%io:write(Values),
timer:sleep(1000),
    plotter:drawValues(Panel,Values),
timer:sleep(1000),

    loop(Tab,PIDS,Panel).

core_init(Tab,X,Panel,Max_Zoom,1000,Env)->
    wx:set_env(Env),
    core_loop(cds:init(Tab,X,Panel,Max_Zoom,1000)).

core_loop(CDS)->
    receive
	{How,Paint,PID}->
	    NCDS=cds:move(How,CDS,Paint)
	%% {zoom,How,From,To}->
	%%     NCDS=cds:zoom(How,From,To,CDS);
	%% {newval,How,Values} ->
	%%     NCDS=cds:update(CDS,How,Values)
    end,
    PID!ok,
    core_loop(NCDS).
	    

timeprint(T1,T2,T3,T4)->
    io:write({1,timer:now_diff(T2,T1)}),
    io:nl(),
    io:write({2,timer:now_diff(T3,T2)}),
    io:nl(),
    io:write({3,timer:now_diff(T4,T3)}),
    io:nl().
    
