-module(plotter).
-compile(export_all).
-include("hijap.hrl").
-define(HEIGHT_FACTOR,2).
-define(MAX_HEIGHT,?HEIGHT_FACTOR*9).
-define(LGrey,?wxLIGHT_GREY_PEN).
-define(Black,?wxBLACK_PEN).
-define(MGrey,?wxMEDIUM_GREY_PEN).
-define(Grey,?wxGREY_PEN).
-define(VERTICAL_INT,100).

clear_canvas(Panel)->
    Paint = wxBufferedPaintDC:new(Panel),
    wxDC:clear(Paint),
    wxBufferedPaintDC:destroy(Paint).

% Core Line:

%   |  |
% | | ||     |
% ||||||     | |   |
% ------------------
% A horizontal line for Y=Yo
% Lines of given length starting from Yo

%% drawValues(Panel,Values)->
%%     Paint = wxBufferedPaintDC:new(Panel),
%%  %   wxDC:clear(Paint),
%%     drawCoreLine(Paint,Values),
%%     wxBufferedPaintDC:destroy(Paint).

drawLinePoints(Paint,List)->
    lists:map(fun({A,B})->wxDC:drawLine(Paint,A,B) end,List).


drawLines(_,[],_,_)->
    ok;
drawLines(Paint,[Length|Lengths],X,Yo)->
    dl(Paint,X,Yo,Length),
    drawLines(Paint,Lengths,X+1,Yo).

dl(Paint,X,Yo,Length)->
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-?HEIGHT_FACTOR*Length}).
    
drawCoreLine(Paint,Lengths,Yo)->
wxDC:setPen(Paint,?Grey),
    wxDC:drawLine(Paint,{0,Yo},{1000,Yo}),
    wxDC:drawLine(Paint,{0,Yo-?MAX_HEIGHT},{1000,Yo-?MAX_HEIGHT}),
wxDC:setPen(Paint,?MGrey),
    drawLines(Paint,Lengths,0,Yo).

drawCoreLines(Paint,CLs)->
    drawCoreLines(Paint,CLs,42).
drawCoreLines(_,[],_)->ok;
drawCoreLines(Paint,[CL|CLs],Y)->
    drawCoreLine(Paint,CL,Y),
    drawCoreLines(Paint,CLs,Y+42).

drawGrid(Paint,{ZOffset,XOffset},{ZoomLvl,XPos},Labels)->

	ZoomFactor = round(math:pow(2,ZoomLvl+ZOffset)),
	Width=1000,
	AbsOffset = XOffset+XPos-?DETS_PACK_SIZE,
	Offset=AbsOffset rem ?VERTICAL_INT,
	wxDC:setPen(Paint,?LGrey),
	lists:map(fun(X)->
					  wxDC:drawLine(Paint,{X,0},{X,600})
			  end,lists:seq(Width-Offset+?VERTICAL_INT,-Offset,-?VERTICAL_INT)),
	lists:map(fun({L,X,N})->
					  wxWindow:move(L,X-20,610),
					  wxStaticText:setLabel(L,
						label_portray(((AbsOffset div ?VERTICAL_INT)+N)*ZoomFactor))			  end, qutils:zip3(Labels,lists:seq(Width-Offset,-Offset,-?VERTICAL_INT))),
	wxDC:setPen(Paint,?MGrey).

create_labels(Frame,Width)->
	lists:map(fun(_)->
					  wxStaticText:new(Frame,?ANY,"")
				end, lists:seq(0,Width div ?VERTICAL_INT)).


label_portray(N)->
	if N < 1000 ->
			form(N,N,"us");
	   N < 1000000 ->
			form(N div 1000,N/1000, "ms");
	   N < 60000000 ->
			form(N div 1000000, N/1000000, "s");
	   true ->
			form(N div 60000000, N/60000000, "m")
	end.

form(A,B,U)->
	AL = integer_to_list(A),
	if (length(AL)==3) or (B==A) ->
			lists:concat([AL,U]);
	    true->
			lists:concat([AL,".",integer_to_list( round((B-A)*math:pow(10,3-length(AL))) ),U])
	end.
