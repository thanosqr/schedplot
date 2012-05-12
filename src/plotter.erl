-module(plotter).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(HEIGHT_FACTOR,2).
-define(MAX_HEIGHT,?HEIGHT_FACTOR*9).
-define(LGrey,?wxLIGHT_GREY_PEN).
-define(Black,?wxBLACK_PEN).
-define(MGrey,?wxMEDIUM_GREY_PEN).
-define(Grey,?wxGREY_PEN).

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

drawGrid(Paint)->
	wxDC:setPen(Paint,?LGrey),
	lists:map(fun(X)->
					  wxDC:drawLine(Paint,{X,0},{X,600})
			  end,lists:seq(0,1000,50)),
	wxDC:setPen(Paint,?MGrey).

