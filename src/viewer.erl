-module(viewer).

%-export([start/1]).
-compile(export_all).
-include("hijap.hrl").

-record(state, {xpos = 1,
                fromCore = 0,
                zpos,
                vzoom = 16/127,
                coren,
                left_data,
                right_data,
                zoomin_data,
                zoomout_data,
                data,
                frame,
                panel,
                width,
                height,
                labels = [],
                zoom_label,
                schedlabels,
                scarlet,
                fitZoom}).

-define(HEIGHT_INT,42). % graph height + space between cores in px
-define(ZOOM_BUFFER,2).
-define(UNIT_WIDTH,42).
-define(PANEL_SIZE,6).
-define(MAX_OFFSET,1000).
-define(MIN_OFFSET,-1000).
-define(STEP_ALT,10).
-define(STEP_NORM,50).
-define(STEP_CMD,200).
-define(CONTROLS,[left_up,mousewheel,left_dclick,key_down,size]).

-define(EXIT,?wxID_EXIT).

-define(QUIT,#wx{id=?EXIT,event=#wxCommand{type=command_menu_selected}}).

-spec start(qijap:folder()) -> 'ok'.
start(FolderName)->
    {Frame,Panel} = wx_init(),
    Width = ?WIDTH,
    Height = ?HEIGHT,
    Options = [{access, read}],
    Prefix = FolderName ++ "/analyzed_trace",
    {ok, HTab} = dets:open_file(Prefix ++ "1", Options),
    [{init_state,Max_Zoom,CoreN}] = dets:lookup(HTab, init_state),
    Zoom=Max_Zoom-trunc(math:log(Width)/math:log(2)), %%zoom_make
    TTabs = lists:map(fun(CoreID)->
                              FName = Prefix ++ integer_to_list(CoreID),
                              {ok, Tab} = dets:open_file(FName, Options),
                              Tab
                      end, lists:seq(2, CoreN)),
    Tabs = [HTab|TTabs],
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
                    left_data = Width,
                    right_data = Width,
                    zoomin_data = Zoom,
                    zoomout_data = 3,
                    data = Tabs,
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
    draw(State),
    loop(State).

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
    wxFrame:connect(Frame,command_menu_selected),
    wxFrame:connect(Frame,size),
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
    draw(State,Paint),
    wxBufferedPaintDC:destroy(Paint),
    update_zoom_label(State).
   
draw(State,Paint) ->
    ok = requestData(State),
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
    Data = receiveData(),
    wxplot:drawGraph(Paint,Data,
                     State#state.schedlabels,
                     State#state.width+?PW_DIFF,
                     State#state.height+?PH_DIFF,
                     State#state.vzoom).

requestData(State)->
    Tabs  = State#state.data,
    Xpos  = State#state.xpos,
    Zpos  = State#state.zpos,
    Width = State#state.width,
    Self  = self(),
    spawn(fun()->
                  getData(Self,Tabs,Zpos,Xpos,Width)
          end),
    ok.

getData(PID,Tabs,Zpos,Xpos,Width)->
    Key = (Xpos div (?DETS_PACK_SIZE+1))+1,       
    ListIndex = Xpos rem (?DETS_PACK_SIZE+1),
    RawData = if ListIndex + Width =< ?DETS_PACK_SIZE ->
                      lists:map(fun(CoreTab)->
                                        cets:lookup(CoreTab,{Zpos,Key})
                                end, Tabs);
                 true ->
                      lists:map(fun(CoreTab)->
                                        lists:append(
                                          cets:lookup(CoreTab,{Zpos,Key}),
                                          cets:lookup(CoreTab,{Zpos,Key+1})
                                         )
                                end, Tabs)
              end,
    Data = lists:map(fun(CoreData)->
                             qutils:sublist(CoreData,ListIndex,Width)
                     end,RawData),
    PID ! {data,Data}. 

receiveData()->
    receive
        {data, Data} -> Data
    end.
            
    

loop(State)->
    receive
        ?QUIT ->
            wxWindow:close(State#state.frame,[]),
            ok = wx:destroy(),
            ok;
        #wx{} = WxEvent ->
            NewState = case change_state(WxEvent,State) of
                           same -> State;
                           NState ->
                               draw(NState),
                               NState
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
            if State#state.left_data>=Step ->
                    State#state{xpos = Xpos - Step,
                                left_data=State#state.left_data - Step,
                                right_data=State#state.right_data + Step};
               true -> same
            end;
        {right, Step}->
            if State#state.right_data>=Step ->
                    State#state{xpos = Xpos + Step,
                                left_data=State#state.left_data + Step,
                                right_data=State#state.right_data - Step};
               true -> same
            end;

        zoom_in->
            if State#state.zoomin_data>0 ->
                    State#state{zpos = Zpos-1,
                                xpos = Xpos*2-1,
                                left_data=2*State#state.left_data,
                                right_data=2*State#state.right_data,
                                zoomin_data=State#state.zoomin_data-1,
                                zoomout_data=State#state.zoomout_data+1};
               true -> same
            end;
        zoom_out->
            if State#state.zoomout_data>0 ->
                    State#state{zpos = Zpos+1,
                                xpos = (Xpos +1) div 2,
                                left_data=State#state.left_data div 2,
                                right_data=State#state.right_data div 2,
                                zoomin_data=State#state.zoomin_data+1,
                                zoomout_data=State#state.zoomout_data-1};
               true -> same
            end;
        reset->
            State#state{ zpos = State#state.fitZoom,
                         xpos = 1,
                         fromCore = 0,
                         vzoom = 16/127,
                         left_data = State#state.width,
                         right_data = State#state.width,
                         zoomin_data = State#state.fitZoom,
                         zoomout_data = 3,
                         width = State#state.width,
                         height = State#state.height
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
    {resize,W,H};
change_decode(#wx{event=#wxKey{keyCode=80}})->
    print;
change_decode(_) ->
    same.

create_labels(Frame,N)->
    lists:map(fun(_)->
		      wxStaticText:new(Frame,?ANY,"")
	      end, lists:seq(0,N)).
