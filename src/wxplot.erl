-module(wxplot).

-export([drawScarlet/6,
         drawGrid/6, 
         drawGraph/6,
         label_portray/1,
         highlight/4]).

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

-define(HLIGHT, ?wxCYAN_PEN).
-define(DEF_C,?MGrey).

-spec highlight(wxpaint(), non_neg_integer(),
                non_neg_integer(), non_neg_integer()) -> 'ok'.
highlight(Paint, X1, X2, Y) ->
    wxDC:setPen(Paint,?HLIGHT),
    wxDC:drawRectangle(Paint,{X1,0,X2-X1,Y}),
    wxDC:drawRectangle(Paint,{X1+1,1,X2-X1-2,Y-2}),
    wxDC:setPen(Paint,?DEF_C).
    

-spec label_portray(integer()) -> qijap:label().
label_portray(N) when N < 0 ->
    "-" ++ label_portray(-N);
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

-spec drawScarlet(wxpaint(), ibap:core_id(),[wxlabel()],
			  non_neg_integer(), non_neg_integer(), 
                  non_neg_integer())->
                         'ok'.
drawScarlet(Paint,FromCore,Scarlets,Zpos,Xpos,VertZ)->
    wxDC:setPen(Paint,?Red),
    wxDC:setFont(Paint,wxFont:new(10,?wxFONTFAMILY_SWISS,
                                  ?wxFONTSTYLE_NORMAL,
                                  ?wxFONTWEIGHT_NORMAL)),

    ZF = round(math:pow(2,Zpos+?DEF_GU)),
    GraphHeight = qutils:strunc(VertZ*?GRAPH_HEIGHT),
    Ydiff = GraphHeight + (?SPACE_BETW_CORES div 2),
    Yfactor  = GraphHeight + ?SPACE_BETW_CORES,
    lists:foreach(fun({Time,{SID,Label}})->
                          if SID>FromCore ->
                                  Y  = (SID-FromCore)*Yfactor,
                                  Y2 = Y - Ydiff,
                                  XN = (Time div ZF) - Xpos,
                                  wxDC:drawLine(Paint, {XN,Y+2}, {XN,Y2}),
                                  wxDC:drawLabel(Paint, Label, {XN+2,Y2-6,42,42});
                             true ->
                                  ok
                          end
                  end,Scarlets),
    wxDC:setPen(Paint,?DEF_C).


-spec drawGrid(wxpaint(), non_neg_integer(),non_neg_integer(),
               [wxlabel()],non_neg_integer(),non_neg_integer()) ->   'ok'.
drawGrid(Paint,Zpos,Xpos,Labels,PHeight,PWidth)->
    ZoomFactor = round(math:pow(2,Zpos+?DEF_GU)), %edit
    Offset=Xpos rem ?VERTICAL_INT,
    wxDC:setPen(Paint,?LGrey),
    lists:foreach(fun(X)->
		      wxDC:drawLine(Paint,{X,0},{X,PHeight})
 	      end,lists:seq(-Offset,PWidth-Offset+?VERTICAL_INT,?VERTICAL_INT)),
    lists:foreach(fun({L,X,N})->
		      Label_N = ((Xpos div ?VERTICAL_INT)+N)*?VERTICAL_INT*ZoomFactor,
		      wxWindow:move(L,X+21,PHeight+10),
		      wxStaticText:setLabel(L,label_portray(Label_N))
	      end, qutils:zip3(Labels,lists:seq(-Offset,PWidth-Offset,?VERTICAL_INT))),
    wxDC:setPen(Paint,?DEF_C).



-spec drawGraph(wxpaint(), [cets:seven_bit_list()],[wxlabel()],
					non_neg_integer(),non_neg_integer(),float()) -> 'ok'.

drawGraph(Paint,Values,SchedLabels,PWidth,PHeight,VertZ)->
    drawGraph(Paint,Values, SchedLabels,
              qutils:strunc(VertZ*?GRAPH_HEIGHT)+?SPACE_BETW_CORES,
              PWidth,PHeight,VertZ).

drawGraph(_,[],_,_,_,_,_)->ok;
drawGraph(Paint,[Lines|RestLines],SchedLabels,Y,PWidth,PHeight,VertZ)->
    drawCoreLine(Paint,Lines,Y,PWidth,VertZ),
    Yinterval = qutils:strunc(VertZ*?GRAPH_HEIGHT) +?SPACE_BETW_CORES,
    RestLabels = update_sched_label(SchedLabels,Yinterval,PHeight,Y),
    drawGraph(Paint,RestLines,RestLabels,Y+Yinterval,PWidth,PHeight,VertZ).

update_sched_label([],_,_,_)->[];
update_sched_label([L|Ls],Yinterval,PHeight,Y)->
    {XL,YL} = if Y-?SPACE_BETW_CORES<PHeight->
                      {8, Y-(Yinterval div 2)};
                 true->
                      {-42,-42}
              end,
    wxWindow:move(L,XL,YL),
    Ls.
    

drawCoreLine(Paint,Lengths,Yo,PWidth,VertZ)->
    wxDC:setPen(Paint,?Grey),
    wxDC:drawLine(Paint,{0,Yo},{PWidth,Yo}),
    Y = Yo-qutils:strunc(VertZ*?GRAPH_HEIGHT),
    wxDC:drawLine(Paint,{0,Y},{PWidth,Y}),
    wxDC:setPen(Paint,?DEF_C),
    drawLines(Paint,Lengths,0,Yo,VertZ).

drawLines(_,[],_,_,_) -> ok;
drawLines(Paint,[Length|Lengths],X,Yo,VertZ)->
    wxDC:drawLine(Paint,{X,Yo},{X,Yo-qutils:strunc(VertZ*Length)}),
    drawLines(Paint,Lengths,X+1,Yo,VertZ).
