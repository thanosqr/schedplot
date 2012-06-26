-module(plotter).

-export([create_labels/2,scarlet/6,
	 drawGrid/6, drawCoreLines/6,
	 label_portray/1]).

-export_type([wxpaint/0, wxpanel/0, wxframe/0, 
			  wxlabel/0]).

-type wxframe() :: any().
-type wxpanel() :: any().
-type wxpaint() :: any().
-type wxlabel() :: any().

-include("hijap.hrl").

-define(GRAPH_HEIGHT,127).
-define(SPACE_BETW_CORES,25).
-define(LGrey,?wxLIGHT_GREY_PEN).
-define(Black,?wxBLACK_PEN).
-define(MGrey,?wxMEDIUM_GREY_PEN).
-define(Grey,?wxGREY_PEN).
-define(Red,?wxRED_PEN).

-define(DEF_C,?MGrey).

%% clear_canvas(Panel)->
%%     Paint = wxBufferedPaintDC:new(Panel),
%%     wxDC:clear(Paint),
%%     wxBufferedPaintDC:destroy(Paint).

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

%% drawLinePoints(Paint,List)->
%%     lists:map(fun({A,B})->wxDC:drawLine(Paint,A,B) end,List).


drawLines(_,[],_,_,_)->
    ok;
drawLines(Paint,[Length|Lengths],X,Yo,VertZ)->
    dl(Paint,X,Yo,Length,VertZ),
    drawLines(Paint,Lengths,X+1,Yo,VertZ).

dl(Paint,X,Yo,Length,VertZ)->
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-qutils:strunc(VertZ*Length)}).

drawCoreLine(Paint,Lengths,Yo,PWidth,VertZ)->
    wxDC:setPen(Paint,?Grey),
    wxDC:drawLine(Paint,{0,Yo},{PWidth,Yo}),
    wxDC:drawLine(Paint,{0,Yo-qutils:strunc(VertZ*?GRAPH_HEIGHT)},
		  {PWidth,Yo-qutils:strunc(VertZ*?GRAPH_HEIGHT)}),
    wxDC:setPen(Paint,?DEF_C),
    drawLines(Paint,Lengths,0,Yo,VertZ).

-spec drawCoreLines(wxpaint(), [cets:seven_bit_list()],[wxlabel],
					non_neg_integer(),
					non_neg_integer(),
					non_neg_integer()) ->
						   'ok'.
drawCoreLines(Paint,CLs,SLs,PWidth,PHeight,VertZ)->
    drawCoreLines(Paint,CLs,qutils:strunc(VertZ*?GRAPH_HEIGHT)+?SPACE_BETW_CORES,
		  SLs,PWidth,PHeight,VertZ).
drawCoreLines(_,[],_,_,_,_,_)->ok;
drawCoreLines(Paint,[CL|CLs],Y,[SL|SLs],PWidth,PHeight,VertZ)->
    drawCoreLine(Paint,CL,Y,PWidth,VertZ),
    if Y-?SPACE_BETW_CORES<PHeight->
	    wxWindow:move(SL,8,Y-((qutils:strunc(VertZ*?GRAPH_HEIGHT) +?SPACE_BETW_CORES) div 2));
       true->
	    wxWindow:move(SL,-42,-42)
    end,
    drawCoreLines(Paint,CLs,Y+?SPACE_BETW_CORES+qutils:strunc(VertZ*?GRAPH_HEIGHT),
		  SLs,PWidth,PHeight,VertZ).

-spec drawGrid(wxpaint(), {non_neg_integer(),non_neg_integer()},
				{non_neg_integer(),non_neg_integer()},
				[wxlabel()],non_neg_integer(),non_neg_integer()) ->
					   'ok'.

drawGrid(Paint,{ZOffset,XOffset},{ZoomLvl,XPos},Labels,PHeight,PWidth)->
    ZoomFactor = round(math:pow(2,ZoomLvl+ZOffset+?DEF_GU-1)), %edit
    AbsOffset = XOffset+XPos-?DETS_PACK_SIZE,

    Offset=AbsOffset rem ?VERTICAL_INT,
    wxDC:setPen(Paint,?LGrey),
    lists:foreach(fun(X)->
		      wxDC:drawLine(Paint,{X,0},{X,PHeight})
 	      end,lists:seq(-Offset,PWidth-Offset+?VERTICAL_INT,?VERTICAL_INT)),
    lists:foreach(fun({L,X,N})->
		      Label_N = ((AbsOffset div ?VERTICAL_INT)+N)*?VERTICAL_INT*ZoomFactor,
		      wxWindow:move(L,X+21,PHeight+10),
		      wxStaticText:setLabel(L,label_portray(Label_N))
	      end, qutils:zip3(Labels,lists:seq(-Offset,PWidth-Offset,?VERTICAL_INT))),
    wxDC:setPen(Paint,?DEF_C).

-spec create_labels(wxframe(), non_neg_integer)->
						   [wxlabel()].
create_labels(Frame,N)->
    lists:map(fun(_)->
		      wxStaticText:new(Frame,?ANY,"")
	      end, lists:seq(0,N)).

-spec label_portray(non_neg_integer()) -> qijap:label().
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

-spec scarlet(wxpaint(), ibap:core_id(),[wxlabel()],
			  non_neg_integer(), non_neg_integer(), 
			  non_neg_integer())->
					 'ok'.
scarlet(Paint,FromCore,L,Z,X,VertZ)->
    wxDC:setPen(Paint,?Red),
    wxDC:setFont(Paint,wxFont:new(10,?wxFONTFAMILY_SWISS,
				  ?wxFONTSTYLE_NORMAL,
				  ?wxFONTWEIGHT_NORMAL)),
    lists:foreach(fun({Time,{SID,Label}})->
		      if SID>FromCore ->
			      Y= (SID-FromCore)*(?SPACE_BETW_CORES
						 +qutils:strunc(VertZ*?GRAPH_HEIGHT)),
			      XN = (Time-X) div Z,
			      wxDC:drawLine(Paint,
					    {XN,Y+2},
					    {XN,Y-qutils:strunc(VertZ*?GRAPH_HEIGHT)-(?SPACE_BETW_CORES div 2)}),
			      wxDC:drawLabel(Paint,Label,
					     {XN+2,Y-4-qutils:strunc(VertZ*?GRAPH_HEIGHT)-(?SPACE_BETW_CORES div 2),42,42});
			 true ->
			      ok
		      end
	      end,L),
    wxDC:setPen(Paint,?DEF_C).
