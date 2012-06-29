-module(viewer).

-export([start/1]).
-include("hijap.hrl").

-record(state, {xpos = 1,
                fromCore = 0,
                zpos,
                vzoom = 16/127,
                coren,
                data,
                frame,
                panel,
                width,
                height,
                labels = [],
                zoom_label,
                schedlabels,
                scarlet,
                fitZoom,
                highlight = false}).

-define(HEIGHT_INT,42). % graph height + space between cores in px
-define(ZOOM_BUFFER,2).
-define(UNIT_WIDTH,42).
-define(PANEL_SIZE,6).
-define(MAX_OFFSET,1000).
-define(MIN_OFFSET,-1000).
-define(STEP_ALT,10).
-define(STEP_NORM,50).
-define(STEP_CMD,200).
%-define(CONTROLS,[left_down,left_up,mousewheel,left_dclick,key_down,size]).
-define(CONTROLS,[key_down,size,right_up,right_down]).

-define(EXIT,?wxID_EXIT).

-define(QUIT,#wx{id=?EXIT,event=#wxCommand{type=command_menu_selected}}).

-spec start(file:filename()) -> 'ok'.
start(FolderName)->
    {Frame,Panel} = wx_init(),
    Width = ?WIDTH,
    Height = ?HEIGHT,
    Options = [{access, read}],
    Prefix = FolderName ++ "/analyzed_trace",
    HBets = bets:open_file(Prefix ++ "1", Options, 1),
    [{init_state,Max_Zoom,CoreN}] = bets:plain_lookup_otr(HBets, init_state),
    Zoom=Max_Zoom-trunc(math:log(Width)/math:log(2)), %%zoom_make
    TBets = lists:map(fun(CoreID)->
                              FName = Prefix ++ integer_to_list(CoreID),
                              bets:open_file(FName, Options, CoreID)
                      end, lists:seq(2, CoreN)),
    Labels = create_labels(Frame,?LABEL_N),
    SchedLabels = create_labels(Frame,CoreN-1),
    lists:foreach(fun({X,L})->
                          wxStaticText:setLabel(L, "S"++integer_to_list(X))
                  end, lists:zip(lists:seq(1,CoreN),SchedLabels)),
    {ok,S} = file:open(FolderName ++ "/trace_gabi_header", [read]),
    {ok,_} = io:read(S, ''),
    case io:read(S, '') of
        {ok, false} ->
            ok;
        {ok, true} ->
            wxStaticText:setLabel(lists:last(SchedLabels), "GC")
    end,
    Zoom_Label = wxStaticText:new(Frame,?ANY,"",
                                  [{pos,{?PWIDTH+?ZLW,?PHEIGHT+?ZLH}}]),
    State = #state{ zpos  = Zoom,
                    coren = CoreN,
                    data = [HBets|TBets],
                    frame = Frame,
                    panel = Panel,
                    width = Width,
                    height = Height,
                    labels = Labels,
                    zoom_label = Zoom_Label,
                    schedlabels = SchedLabels,
                    scarlet = scarlet:open(FolderName),
                    fitZoom = Zoom
                  },
    NState = draw(State),
    loop(NState).

wx_init()->
    Wx=wx:new(),
    Frame=wxFrame:new(Wx,?ANY,"",[{size,{?WIDTH,?HEIGHT}}]),  
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File,?EXIT,"Quit"),
    wxMenuBar:append(MenuBar,File,"&File"),
    wxFrame:setMenuBar(Frame,MenuBar),
    wxFrame:show(Frame),
    Panel = create_panel(Frame,?PWIDTH,?PHEIGHT),
    lists:foreach(fun(XX) ->
                          wxEvtHandler:connect(Frame,XX) 
                  end, [command_menu_selected,size]),
    {Frame,Panel}.
                    
create_panel(Frame,W,H)->
    Panel = wxPanel:new(Frame,[{size,{W,H}},{pos,{42,0}}]),
    lists:foreach(fun(XX) ->
                          wxEvtHandler:connect(Panel,XX) 
                  end, ?CONTROLS),
    Panel.

draw(State) ->
    Paint = wxBufferedPaintDC:new(State#state.panel),    
    wxDC:clear(Paint),
    NState = draw(State,Paint),
    wxBufferedPaintDC:destroy(Paint),
    update_zoom_label(State),
    NState.
   
draw(State,Paint) ->
    case State#state.highlight of
        false   -> ok;
        {X1,X2} -> wxplot:highlight(Paint,X1,X2,
                                    State#state.height+?PH_DIFF)
    end,
    wxplot:drawGrid(Paint,
                    State#state.zpos,
                    State#state.xpos,
                    State#state.labels,
                    State#state.height+?PH_DIFF,
                    State#state.width+?PW_DIFF),
    VScarlet = scarlet:get_visible(State#state.zpos,
                                   State#state.xpos,
                                   State#state.width,
                                   State#state.scarlet),
    wxplot:drawScarlet(Paint,
                       State#state.fromCore,
                       VScarlet,
                       State#state.zpos,
                       State#state.xpos,
                       State#state.vzoom),                   
    {NBetss, Data} = getData(State),
    wxplot:drawGraph(Paint,Data,
                     State#state.schedlabels,
                     State#state.width+?PW_DIFF,
                     State#state.height+?PH_DIFF,
                     State#state.vzoom),
    State#state{data = NBetss}.


getData(State)->
    Betss  = State#state.data,
    Xpos  = State#state.xpos,
    Zpos  = State#state.zpos,
    Width = State#state.width,
    Key = (Xpos div (?DETS_PACK_SIZE+1))+1,       
    ListIndex = Xpos rem (?DETS_PACK_SIZE+1),
    {NBetss, RawData} = if ListIndex + Width =< ?DETS_PACK_SIZE ->
                                get_from_bets(Betss, {Zpos,Key}, [], [], 1);
                           true ->
                                get_from_bets(Betss, {Zpos,Key}, [], [], 2)
                        end,
    Data = lists:map(fun(CoreData)->
                             qutils:sublist(CoreData,ListIndex,Width)
                     end,RawData),
    {NBetss, Data}.

get_from_bets([], _, Data, Betss,_) -> 
    {lists:reverse(Betss), lists:reverse(Data)};
get_from_bets([Bets|Betss], Key, DataAcc, BetsAcc,1) ->
    {NBets, Data} = bets:lookup(Bets, Key),
    get_from_bets(Betss, Key, [Data|DataAcc], [NBets|BetsAcc],1);
get_from_bets([Bets|Betss], {ZK,XK}, DataAcc, BetsAcc,2) ->
    Data1 = bets:lookup_otr(Bets, {ZK, XK}),
    {NBets, Data2} = bets:lookup(Bets, {ZK, XK+1}),
    Data = Data1 ++ Data2,
    get_from_bets(Betss, {ZK,XK}, [Data|DataAcc], [NBets|BetsAcc],2).


loop(State)->
    receive
        ?QUIT ->
            lists:foreach(fun(Bets) -> 
                                  bets:close(Bets)
                          end, State#state.data),
            wxWindow:close(State#state.frame,[]),
            ok = wx:destroy(),
            ok;
        #wx{} = WxEvent ->
            NewState = case change_state(WxEvent,State) of
                           same -> State;
                           NState ->
                               draw(NState)
                       end,
            loop(NewState)
    end.

update_zoom_label(State) ->
    CoreN = State#state.coren,
    Zoom = State#state.zpos,
    Label = State#state.zoom_label,
    W = State#state.width,
    H = State#state.height,
    ZF = round(math:pow(2, Zoom + ?DEF_GU)),
    wxWindow:move(Label,W+?ZLW,H+?ZLH),
    wxStaticText:setLabel(Label,
                          "Zoom 1:" ++ integer_to_list(ZF) ++ "  [1px = " ++
                              wxplot:label_portray(ZF) ++ "]" ++
                              "  Cores: " ++ integer_to_list(CoreN)).

hide_extra_sched_labels(0,_)->ok;
hide_extra_sched_labels(FromCore,[Label|Labels])->
    wxWindow:move(Label,-42,-42),
    hide_extra_sched_labels(FromCore-1,Labels).

change_state(How, State) ->
    Xpos = State#state.xpos,
    Zpos = State#state.zpos,
    case change_decode(How) of
        {resize,W,H}->
            wxPanel:destroy(State#state.panel),
            State#state{width=W,
                        height=H,
                        panel=create_panel(State#state.frame,
                                           W+?PW_DIFF,H+?PH_DIFF)};
        {left, Step}->
            State#state{xpos = Xpos - Step};
        {right, Step}->
            State#state{xpos = Xpos + Step};
        zoom_in->
            case State#state.highlight of
                false ->
                    if Zpos > 0 ->
                            State#state{zpos = Zpos-1, xpos = Xpos*2-1};
                       true -> same
                    end;
                {X1,X2} ->
                    zoom_to_selection(State#state{highlight=false},X1,X2)
            end;
        zoom_out->
            State#state{zpos = Zpos+1, xpos = (Xpos +1) div 2};
        reset->
            State#state{ zpos = State#state.fitZoom,
                         xpos = 1,
                         fromCore = 0,
                         vzoom = 16/127,
                         width = State#state.width,
                         height = State#state.height,
                         highlight = false
                       };
        {core_up,CN}->
            FromCore = State#state.fromCore,
            if FromCore-CN>=0 ->
                    hide_extra_sched_labels(FromCore-CN,State#state.schedlabels),
                    State#state{fromCore=FromCore-CN};
               true->
                    same
            end;
        {core_down,CN}->
            FromCore = State#state.fromCore,
            CoreN = State#state.coren,
            if FromCore+CN<CoreN->
                    hide_extra_sched_labels(FromCore+CN,State#state.schedlabels),
                    State#state{fromCore=FromCore+CN};
               true->
                    same
            end;
        {highlight, X1, X2} ->
            State#state{highlight={X1,X2}};
        highlight_off ->
            State#state{highlight=false};
        print->
            %% {_,_,T}=erlang:now(),
            %% N=lists:concat([atom_to_list(print),integer_to_list(T),atom_to_list('.bmp')]),
            %% B = wxBitmap:new(State#state.width,
            %%             ?PHEIGHT),
            %% M = wxMemoryDC:new(),
            %% wxMemoryDC:selectObject(M,B),
            %% P = wxPaintDC:new(State#state.panel),
            %% draw(Datapack,P),
            %% wxDC:setLogicalFunction(P,?wxCOPY),
            %% wxDC:setLogicalFunction(M,?wxCOPY),
            %% wxDC:blit(M,{0,0},{State#state.width,?PHEIGHT},P,{0,0}),
            %% wxBitmap:saveFile(B,N,?wxBITMAP_TYPE_GIF),
            %% wxMemoryDC:destroy(M),
            %% wxBitmap:destroy(B),
            %% io:format("screenshot saved\n"),
            same;
        same->
            same
    end.

zoom_to_selection(State,X1,X2)->
    Zpos   = State#state.zpos,
    Width  = State#state.width,
    NZpos  = max(0,Zpos - trunc(math:log(Width/(X2-X1))/math:log(2))),
    NXpos  = round((State#state.xpos + X1)*math:pow(2,Zpos-NZpos)),
    State#state{zpos = NZpos,
                xpos = NXpos}.
                                
change_decode(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_ADD}})->
    zoom_in;
change_decode(#wx{event=#wxKey{keyCode=61}}) ->
    zoom_in;
change_decode(#wx{event=#wxKey{keyCode=?WXK_NUMPAD_SUBTRACT}}) ->
    zoom_out;
change_decode(#wx{event=#wxKey{keyCode=45}}) ->
    zoom_out;
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT,altDown=true}}) ->
    {left,?STEP_ALT};
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT,controlDown=true}}) ->
    {left,?STEP_CMD};
change_decode(#wx{event=#wxKey{keyCode=?WXK_LEFT}}) ->
    {left,?STEP_NORM};
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT,altDown=true}}) ->
    {right,?STEP_ALT};
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT,controlDown=true}}) ->
    {right,?STEP_CMD};
change_decode(#wx{event=#wxKey{keyCode=?WXK_RIGHT}}) ->
    {right,?STEP_NORM};
change_decode(#wx{event=#wxKey{keyCode=?WXK_HOME}}) ->
    reset;
change_decode(#wx{event=#wxKey{keyCode=?WXK_UP,altDown=true}}) ->
    {core_up,10};
change_decode(#wx{event=#wxKey{keyCode=?WXK_DOWN,altDown=true}}) ->
    {core_down,10};
change_decode(#wx{event=#wxKey{keyCode=?WXK_UP,altDown=false}}) ->
    {core_up,1};
change_decode(#wx{event=#wxKey{keyCode=?WXK_DOWN,altDown=false}}) ->
    {core_down,1};
change_decode(#wx{event=#wxSize{size={W,H}}}) ->
    receive 
        #wx{event=#wxSize{size={W,H}}} = WxE ->
            change_decode(WxE)
    after 21 -> {resize,W,H}
    end;
change_decode(#wx{event=#wxKey{keyCode=80}})->
    print;
change_decode(#wx{event=#wxMouse{rightDown=true, x=X1}})->
    receive
        #wx{event=#wxMouse{rightDown=false, x=X2}}-> ok
    end,
    if X1 == X2 ->
            same;
       X1 > X2 ->
            {highlight, X2, X1};
       X1 < X2 ->
            {highlight, X1, X2}
    end;
change_decode(#wx{event=#wxKey{keyCode=?WXK_ESCAPE}})->
    highlight_off;
change_decode(_) ->
    same.

create_labels(Frame,N)->
    lists:map(fun(_)->
		      wxStaticText:new(Frame,?ANY,"")
	      end, lists:seq(0,N)).
