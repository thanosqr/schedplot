-module(qwg).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

% left_down, left_up, middle_down, middle_up, right_down, right_up, 
% motion, enter_window, leave_window, 
% left_dclick, middle_dclick, right_dclick, mousewheel, 

-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down]).
-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).

-define(STEP,1).

-record(state, {cursorPx=0,          % cursor position (px-int)
		cursorT=0,           % cursor position (us-int)
		t0=0,                % time of first displayed pixel (us-int)
		tEnd,                % time of last pixel (us-int)
		width=800,           % display width
		height=600,          % display height
		zoom=1,              % zoom lvl: 1 = fit screen
		maxy=10,             % height of each core
		duration,            % time on display (us-int)  
		visibleCores,        % cores in display ([int])
		previousCores,       % cores before ([int])
		nextCores,           % cores after ([int])
		max_zoom}).          % max zoom level (int)
	  


init()->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,-1,"",[{size,{800,600}}]),
    Graph=wxPanel:new(Frame),
    init(graph,Graph,Frame).
%    loop(Frame).

init(graph,Panel,Frame)->
    lists:map(fun(X)->
		      wxEvtHandler:connect(Panel,X) 
	      end, ?CONTROLS),
    wxFrame:show(Frame),
    graph_loop(Panel,qdb:init_state(),qdb:init()).
 %   spawn(?MODULE,graph_loop,[Panel]).



loop(_Frame)->
    ok.
graph_loop(Panel,State,Tab)->
    receive
	#wx{event=#wxKey{keyCode=?WXK_DOWN,controlDown=true}}->
	    ChangeMode = move2bottom;
	#wx{event=#wxKey{keyCode=?WXK_UP,controlDown=true}}->
	    ChangeMode = move2top;
	#wx{event=#wxKey{keyCode=?WXK_LEFT,controlDown=true}}->
	    ChangeMode = move2first;
	#wx{event=#wxKey{keyCode=?WXK_RIGHT,controlDown=true}}->
	    ChangeMode = move2last;

	#wx{event=#wxKey{keyCode=?WXK_DOWN}}->
	    ChangeMode = move2down;
	#wx{event=#wxKey{keyCode=?WXK_UP}}->
	    ChangeMode = move2up;
	#wx{event=#wxKey{keyCode=?WXK_LEFT}}->
	    ChangeMode = move2left;
	#wx{event=#wxKey{keyCode=?WXK_RIGHT}}->
	    ChangeMode = move2right;

	#wx{event=#wxMouse{type=left_up,x=X}}->
	    ChangeMode = {place_cursor,X};

	#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD,controlDown=true}}->
	    ChangeMode = real_size;
	#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT,controlDown=true}}->
	    ChangeMode = fit_screen;

	#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}}->
	    ChangeMode = zoom_in;
	#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}->
	    ChangeMode = zoom_out
						
%	#wx{event=#wxMouse{type=mousewheel,wheelRotation=Rot}}->
%	    move(Rot)
    end,
    NewState=change_state(ChangeMode,State),
    io:write('>>'),
    
io:write({State#state.t0,State#state.duration}),
io:write({NewState#state.t0,NewState#state.duration}),
io:nl(),
    qdb:draw_state(NewState,Panel,Tab),
    graph_loop(Panel,NewState,Tab).

change_state(zoom_out,State)->
    Zoom = State#state.zoom,
    if Zoom > 1 ->
	    New0CursorTimeDiff = round(
		(State#state.cursorPx * State#state.duration)/(2*State#state.width)),
	    State#state{zoom=Zoom-1, 
			t0 = max(State#state.cursorT - New0CursorTimeDiff,0),
			duration = round(State#state.duration / 2)};
       Zoom == 1 ->
	    State
    end;
change_state(zoom_in,State)->
    Zoom = State#state.zoom,
    Max = State#state.max_zoom,
    if Zoom < Max ->
	    New0CursorTimeDiff = round(
		(2*State#state.cursorPx * State#state.duration)/State#state.width),
	    State#state{zoom=Zoom+1, 
			t0 = max(State#state.cursorT - New0CursorTimeDiff,0),
			duration = 2*State#state.duration};
       Zoom == Max ->
	    State
    end;

change_state(real_size,State)->
    State#state{zoom = State#state.max_zoom,
		duration = State#state.width,
		t0 = State#state.cursorT - State#state.cursorPx};

change_state(fit_screen,State)->
    State#state{zoom=1,
		duration = State#state.tEnd,
		t0=0,
		cursorPx=0,
		cursorT=0};

change_state({place_cursor,X},State)->
    State#state{cursorPx=X, cursorT=
		    State#state.t0 + 
		    round(State#state.cursorPx*State#state.duration
		              /State#state.width)
	       };

change_state(move2right,State) when State#state.t0+State#state.duration < State#state.tEnd->
    if State#state.t0 + ?STEP =< State#state.cursorT ->
	    State#state{t0=State#state.t0+?STEP};
       true ->
	    State#state{t0=State#state.t0+?STEP,
		       cursorPx=0,
		       cursorT=State#state.t0+?STEP}
    end;
change_state(move2right,State)->
    State;

change_state(move2left,State) when State#state.t0>0->
    if State#state.t0 + State#state.duration - ?STEP >= State#state.cursorT ->
	    State#state{t0=State#state.t0-?STEP};
       true ->
	    State#state{t0=State#state.t0-?STEP,
			cursorPx=State#state.width,
			cursorT=State#state.t0+State#state.duration-?STEP}
    end;
change_state(move2left,State) ->
    State;
change_state(move2up,State) when State#state.previousCores==[]->
    State;
change_state(move2up,State) ->
    {PCs,PC} = select_tail(State#state.previousCores),
    {VCs,VC} = select_tail(State#state.visibleCores),
    NCs = State#state.nextCores,
    State#state{
      previousCores = PCs,     
      visibleCores = [PC|VCs],
      nextCores = [VC|NCs]};

change_state(move2down,State) when State#state.nextCores==[] ->
    State;
change_state(move2down,State)->
    PCs = State#state.previousCores,
    [VC|VCs] = State#state.visibleCores,
    [NC|NCs] = State#state.nextCores,
    State#state{
      previousCores = lists:append(PCs,[VC]),
      visibleCores = lists:append(VCs,[NC]),
      nextCores = NCs};


change_state(move2first,State)->
    State#state{
      t0 = 0,
      cursorPx = 0,
      cursorT = 0
     };

change_state(move2last,State) ->
    New_t0 = State#state.tEnd - State#state.duration,
    State#state{
      t0 = New_t0,
      cursorT = New_t0,
      cursorPx = State#state.cursorT * State#state.duration / State#state.width
     };

change_state(move2top,State) ->
    N = length(State#state.visibleCores),
    case take(N,State#state.previousCores) of
	{L,ok,Rest} ->
	    State#state{ visibleCores=L,
			 previousCores=[],
			 nextCores=lists:append(Rest,State#state.nextCores)};
	{L1,M} ->
	    {L2,ok,Rest}=take(M,State#state.visibleCores),
	    State#state{ visibleCores=lists:append(L1,L2),
			 previousCores=[],
			 nextCores=lists:append(Rest,State#state.nextCores)}
    end;

change_state(move2bottom,State) ->
    N = length(State#state.visibleCores),
    case take_from_tail(N,State#state.nextCores) of
	{L,ok,Rest} ->
	    State#state{ visibleCores=L,
			 nextCores=[],
			 previousCores=lists:append(State#state.nextCores,Rest)};
	{L1,M} ->
	    {L2,ok,Rest}=take_from_tail(M,State#state.visibleCores),
	    State#state{ visibleCores=lists:append(L2,L1),
			 nextCores=[],
			 previousCores=lists:append(State#state.nextCores,Rest)}
    end.

%change_state(add_core(ID),State)->
%change_state(remove_core(ID),State)->
%change_state(reorder_cores(IDs),State)->

%change_state(new_window_width(X))->
%change_state(new_window_heigh(Y))->
    
%change_state(vertical_zoom_in,State) ->
%change_state(vertical_zoom_out,State) ->
%change_state(all_cores,State) ->
%change_state(one_core,State) ->


draw_state(_State,_Panel)->
    ok.



select_tail(L) ->
    select_tail(L,[]).
select_tail([T],L)->
    {L,T};
select_tail([H|T],L)->
    select_tail(T,[H|L]).

take(N,L)->
    take(N,L,[]).
take(0,Rest,T)->
    {lists:reverse(T),ok,Rest};
take(M,[],T) ->
    {lists:reverse(T),M};
take(N,[H|T],Acc) ->
    take(N-1,T,[H|Acc]).

take_from_tail(N,L)->
    RL = lists:reverse(L),
    case take(N,RL) of
	{T,ok,Rest}->
	    {lists:reverse(T),ok,lists:reverse(Rest)};
	{T,M} ->
	    {lists:reverse(T),M}
    end.
		
