-module(plotter).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

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
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-Length}).
    
dl(_,_,_,_,0)->ok;
dl(Paint,X,Yo,Length,Width)->
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-Length}),
    dl(Paint,X+1,Yo,Length,Width-1).

drawCoreLine(Paint,Lengths,Yo)->
    wxDC:drawLine(Paint,{0,Yo},{1000,Yo}),
    drawLines(Paint,Lengths,-10,Yo).

drawCoreLines(Paint,CLs)->
    drawCoreLines(Paint,CLs,42).
drawCoreLines(_,[],_)->ok;
drawCoreLines(Paint,[CL|CLs],Y)->
    drawCoreLine(Paint,CL,Y),
    drawCoreLines(Paint,CLs,Y+42).

