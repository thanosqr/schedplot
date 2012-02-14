-module(qdb).
-compile(export_all).


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

-record(dbval, {value}).
	  
% {{Time,Zoom,Core},Value}
make_table()->
    {ok,Tab} = dets:open_file(db,[{file,testtable}]),
    D=make(0),
    DF=lists:flatten(D),
    dets:insert(Tab,DF),
    dets:close(Tab).

mt()->
    {ok,Tab} = dets:open_file(db,[{file,testtable}]),
    dets:insert(Tab,[
		     {{0,3,1},{dbval,10}},
		     {{1,3,1},{dbval,20}},
		     {{2,3,1},{dbval,30}},
		     {{3,3,1},{dbval,40}},
		     {{0,2,1},{dbval,15}},
		     {{1,2,1},{dbval,35}},
		     {{0,1,1},{dbval,25}}
		    ]),
    dets:close(Tab).

make(_,_,-2)->[];
make(_,_,-1)->[];
make(Zoom,Core,Now)->
    [{{Now,Zoom,Core},{dbval,(Now+Core)+2*Zoom}},
     {{Now+1,Zoom,Core},{dbval,(Now+Core)+2*Zoom}}
     |make(Zoom,Core,Now-2)].

make_c(_Zoom,5)->
    [];
make_c(Zoom,C) ->
    [make(Zoom,C,4*round(math:pow(2,Zoom)))|
     make_c(Zoom,C+1)].
make(5)->
    [];
make(N)->
    [make_c(N,0)|make(N+1)].

    
init()->
    init(testtable).

init(FileName)->
    {ok,Tab} = dets:open_file(db,[{access,read},{file,FileName}]),
    Tab.

init_state()->
    #state{tEnd=40,duration=4,visibleCores=[1,2],nextCores=[3,4],max_zoom=3,zoom=2}.

get_values(_,_,[],_,_)->[];
get_values(Tab,Zoom,[C|Cores],From,To)->
    [get_values_time(Tab,Zoom,C,From,To)|
     get_values(Tab,Zoom,Cores,From,To)].
get_values_time(Tab,Zoom,Core,From,From)->
    [get_value(Tab,Zoom,Core,From)];
get_values_time(Tab,Zoom,Core,From,To)->
    [get_value(Tab,Zoom,Core,From)|
     get_values_time(Tab,Zoom,Core,From+1,To)].

get_value(Tab,Zoom,Core,Time)->
    io:write({Time,Zoom,Core}),
    [{_,V}] = dets:lookup(Tab,{Time,Zoom,Core}),
    V#dbval.value.
    

test(X)->
    X#dbval.value.
draw_state(State,Panel,Tab)->
    From = State#state.t0,
    To = From + State#state.duration,
    Zoom = State#state.zoom,
    Cores = State#state.visibleCores,
    Lines=get_values(Tab,Zoom,Cores,From,To),
io:nl(),
    Paint = wxPaintDC:new(Panel),
    wxDC:clear(Paint),
    drawCoreLines(Paint,Lines),
    wxPaintDC:destroy(Paint).

drawLinePoints(Paint,List)->
    lists:map(fun({A,B})->wxDC:drawLine(Paint,A,B) end,List).

%Core Line:

%   |  |
% | | ||     |
% ||||||     | |   |
% ------------------
% A horizontal line for Y=Yo
% Lines of given length starting from Yo


drawLines(_,[],_,_)->
    ok;
drawLines(Paint,[Length|Lengths],X,Yo)->
    dl(Paint,X,Yo,Length,42),
    drawLines(Paint,Lengths,X+42,Yo).

dl(_,_,_,_,0)->ok;
dl(Paint,X,Yo,Length,Width)->
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-Length}),
    dl(Paint,X+1,Yo,Length,Width-1).

drawCoreLine(Paint,Lengths,Yo)->
    wxDC:drawLine(Paint,{0,Yo},{800,Yo}),
    drawLines(Paint,Lengths,0,Yo).

drawCoreLines(Paint,CLs)->
    drawCoreLines(Paint,CLs,42).
drawCoreLines(_,[],_)->ok;
drawCoreLines(Paint,[CL|CLs],Y)->
    drawCoreLine(Paint,CL,Y),
    drawCoreLines(Paint,CLs,Y+37).
