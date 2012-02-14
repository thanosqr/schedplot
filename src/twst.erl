-module(twst).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

init()->
    spawn(twst,start,[]).

init(L,VZ)->
    P = init(),
    P!{L,VZ},
    P.

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "", [{size, {800, 600}}]),
    loop(Frame).

loop(Frame)->
    receive
	exit->
	    ok;
	{List,VZ} ->
	    drawLines(Frame,List,VZ),
	    loop(Frame)
    end.



drawLines(Frame,List,VZ)->
    MaxHeight=30,
    MaxList=VZ,
    Factor=MaxHeight/MaxList,
    NList=lists:map(fun(X)->
			    lists:map(fun(Y)->
					      round(Y*Factor)
				      end,X)
		    end,List),
    OnPaint = fun(_Evt, _Obj) ->
		      Paint = wxPaintDC:new(Frame),
		      drawCoreLines(Paint,NList),
		      wxPaintDC:destroy(Paint)
	      end,
    wxFrame:connect(Frame, paint, [{callback, OnPaint}]),
    wxFrame:show(Frame).

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
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-Length}),
    drawLines(Paint,Lengths,X+1,Yo).


drawCoreLine(Paint,Lengths,Yo)->
    wxDC:drawLine(Paint,{0,Yo},{800,Yo}),
    drawLines(Paint,Lengths,0,Yo).

drawCoreLines(Paint,CLs)->
    drawCoreLines(Paint,CLs,42).
drawCoreLines(_,[],_)->ok;
drawCoreLines(Paint,[CL|CLs],Y)->
    drawCoreLine(Paint,CL,Y),
    drawCoreLines(Paint,CLs,Y+37).

test()->
    twst:init([
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100],
	       [10,20,10,20,20,20,42,42,42,40,50,100]
	      ]).

