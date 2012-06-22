-module(plotter).
-compile(export_all).
-include("hijap.hrl").
-define(HEIGHT_FACTOR,2).
-define(MAX_HEIGHT,?HEIGHT_FACTOR*8).
-define(LGrey,?wxLIGHT_GREY_PEN).
-define(Black,?wxBLACK_PEN).
-define(MGrey,?wxMEDIUM_GREY_PEN).
-define(Grey,?wxGREY_PEN).
-define(Red,?wxRED_PEN).

-define(DEF_C,?MGrey).

clear_canvas(Panel)->
    Paint = wxBufferedPaintDC:new(Panel),
    wxDC:clear(Paint),
    wxBufferedPaintDC:destroy(Paint).

%% % Core Line:

%% %   |  |
%% % | | ||     |
%% % ||||||     | |   |
%% % ------------------
%% % A horizontal line for Y=Yo
%% % Lines of given length starting from Yo

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

drawCoreLine(Paint,Lengths,Yo,PWidth)->
    wxDC:setPen(Paint,?Grey),
    wxDC:drawLine(Paint,{0,Yo},{PWidth,Yo}),
    wxDC:drawLine(Paint,{0,Yo-?MAX_HEIGHT},{PWidth,Yo-?MAX_HEIGHT}),
    wxDC:setPen(Paint,?DEF_C),
    drawLines(Paint,Lengths,0,Yo).

drawCoreLines(Paint,CLs,SLs,PWidth)->
    drawCoreLines(Paint,CLs,42,SLs,PWidth).
drawCoreLines(_,[],_,_,_)->ok;
drawCoreLines(Paint,[CL|CLs],Y,[SL|SLs],PWidth)->
    drawCoreLine(Paint,CL,Y,PWidth),
    wxWindow:move(SL,8,Y-18),
    drawCoreLines(Paint,CLs,Y+42,SLs,PWidth).

drawGrid(Paint,{ZOffset,XOffset},{ZoomLvl,XPos},Labels,PHeight,PWidth)->
    ZoomFactor = round(math:pow(2,ZoomLvl+ZOffset+?DEF_GU-1)), %edit
    AbsOffset = XOffset+XPos-?DETS_PACK_SIZE,

    Offset=AbsOffset rem ?VERTICAL_INT,
    wxDC:setPen(Paint,?LGrey),
    lists:map(fun(X)->
		      wxDC:drawLine(Paint,{X,0},{X,PHeight})
 	      end,lists:seq(-Offset,PWidth-Offset+?VERTICAL_INT,?VERTICAL_INT)),
    lists:map(fun({L,X,N})->
		      Label_N = ((AbsOffset div ?VERTICAL_INT)+N)*?VERTICAL_INT*ZoomFactor,
		      wxWindow:move(L,X+21,PHeight+10),
		      wxStaticText:setLabel(L,label_portray(Label_N))
	      end, qutils:zip3(Labels,lists:seq(-Offset,PWidth-Offset,?VERTICAL_INT))),
    wxDC:setPen(Paint,?DEF_C).

create_labels(Frame,N)->
    lists:map(fun(_)->
		      wxStaticText:new(Frame,?ANY,"")
	      end, lists:seq(0,N)).


label_portray(N)->
    if N==0 ->
	    form(0,"");
       N < 1000 ->
	    form(N,"us");
       N < 1000*1000 ->
	    form(N/1000, "ms");
       N < 60*1000*1000 ->
	    form(N/(1000*1000), "s");
       true ->
	    form(N/(60*1000*1000), "m")
    end.

form(R,U)->
    AL = integer_to_list(trunc(R)),
    if (length(AL)==3) or (R==trunc(R)) ->
	    lists:concat([AL,U]);
       true->
	    Dec = get_N_dec(R-trunc(R), 3-length(AL)),
	    lists:concat([AL,".",Dec,U])
    end.

%% 48: ascii code of 0
get_N_dec(_,0)->[];
get_N_dec(R,N)->
    [trunc(R*10)+48|get_N_dec(R*10-trunc(R*10),N-1)].

scarlet(Paint,FromCore,L,Z,X)->
    wxDC:setPen(Paint,?Red),
    wxDC:setFont(Paint,wxFont:new(10,?wxFONTFAMILY_SWISS,?wxFONTSTYLE_NORMAL,
			    ?wxFONTWEIGHT_NORMAL)),
    lists:map(fun({Time,{SID,Label}})->
		      if SID>FromCore ->
			      Y= (SID-FromCore)*42-?MAX_HEIGHT,
			      XN = (Time-X) div Z,
			      wxDC:drawLine(Paint,
					    {XN,Y-(?MAX_HEIGHT div 2)},
					    {XN,Y+2+?MAX_HEIGHT}),
			      wxDC:drawLabel(Paint,Label,{XN+2,Y-?MAX_HEIGHT,42,42});
			 true ->
			      ok
		      end
	      end,L),
    wxDC:setPen(Paint,?DEF_C).
