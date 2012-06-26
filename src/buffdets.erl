-module(buffdets).

-export([open/3, read/1, create_buffer/12]).

-include("hijap.hrl").

-define(DEF_BUFFER_X,5).
-define(DEF_BUFFER_Z,5).

%%  [ zoom levels [cores [ data [values]]]]] 

open(FolderName, Panel, Frame) ->
    open(FolderName, ?DEF_BUFFER_X, ?DEF_BUFFER_Z, Panel, Frame).

open(FolderName,BufferXsize,BufferZsize,Panel,Frame)->
    Width = ?WIDTH,
    Options = [{access, read}],
    {ok, HTab} = dets:open_file(FolderName ++ "/analyzed_trace1", Options),
    [{init_state,Max_Zoom,CoreN}] = dets:lookup(HTab, init_state),
    TTabs = lists:map(fun(CoreID)->
			      FName = FolderName ++ "/analyzed_trace" ++ integer_to_list(CoreID),
			      {ok, Tab} = dets:open_file(FName, Options),
			      Tab
		      end, lists:seq(2, CoreN)),
    Tabs = [HTab|TTabs],
    Labels = plotter:create_labels(Frame,?LABEL_N),
    SchedLabels = plotter:create_labels(Frame,CoreN-1),
    lists:foreach(fun({X,L})->
			  wxStaticText:setLabel(L, 
						lists:concat(["S",integer_to_list(X)]))
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
    create_buffer(Tabs,BufferXsize,BufferZsize,
		  CoreN,Panel,Frame,Max_Zoom,
		  Labels,Width,Zoom_Label,
		  SchedLabels,scarlet:open(FolderName)).

getData(Datapack) ->
    {ZoomLvl,From} = Datapack#buffdets.pos,
    Duration = Datapack#buffdets.width,
    Data = Datapack#buffdets.data,
    ArrayID = (From div (?DETS_PACK_SIZE+1)),       % 1-indexed
    ArrayIndex = (From rem (?DETS_PACK_SIZE+1)+1),

    if ArrayIndex+Duration =< ?DETS_PACK_SIZE ->
	    lists:map(fun(CoreData)->
			      DL=lists:nth(ArrayID,CoreData),
			      qutils:sublist(DL,ArrayIndex,Duration)
		      end,lists:nth(ZoomLvl,Data));
       ArrayIndex+Duration > ?DETS_PACK_SIZE ->
	    lists:map(fun(CoreData)->
			      lists:append(
				qutils:sublist(lists:nth(ArrayID,CoreData),
					       ArrayIndex,
					       ?DETS_PACK_SIZE-ArrayIndex),
				qutils:sublist(lists:nth(ArrayID+1,CoreData),1,
					       Duration-?DETS_PACK_SIZE+ArrayIndex))
		      end,lists:nth(ZoomLvl,Data))
    end.

%% ---All Values--------------------------------------------------------
%% |
%% |                            -----Second Buffered Area (b2)-------
%% |                            |                                   |
%% |         ---Buffered Area---|-----(b1)-----------------         |
%% |         |                  |                         |         |
%% |         |   ----No need to update buffered area----  |         |
%% |         |   |              |         (nu)         |  |         |
%% |         |   |              |     *                |  |         |
%% |         |   ---------------|-----------------------  |         |
%% |         |          *       |       *                 |         |
%% |         -------------------|--------------------------         |
%% |                            |                               *   |
%% |          ?                 -------------------------------------
%% |
%% ----------------------------------------------------------------------

read(Datapack)->
    {lists:nthtail(Datapack#buffdets.fromCore,
		   getData(Datapack)),
     case category(Datapack) of
	 nu ->
	     Datapack;
	 Adj ->
	     Self = self(),
	     spawn(fun()->
			   update(Self,Adj,Datapack)
		   end),
	     Datapack#buffdets{mode=update}
     end}.

update(PID,Adj,Old)->
    Buffer=refresh_buffer(Adj,Old),
    PID!{new_buffer,Buffer}.	


%% buffer_jump(Buffer,From,To)->
%%     {BufferXsize,BufferZsize,_CoreN}=Buffer#buffdets.static,
%%     X = (From div ?DETS_PACK_SIZE)*?DETS_PACK_SIZE,
%%     Zoom=trunc(math:log((To-X)/Buffer#buffdets.width)/math:log(2)),
%%     refresh_buffer({0,0},
%% 		   Buffer#buffdets{left_data=1000,
%% 				    right_data=1000,
%% 				    zoomin_data=Zoom,
%% 				    zoomout_data=Buffer#buffdets.max_zoom-Zoom,
%% 				    pos={1+BufferZsize div 2,
%% 					 (1+(BufferXsize div 2))*?DETS_PACK_SIZE},
%% 				    offset={Zoom-(BufferZsize div 2),
%% 					    X-(BufferXsize div 2)*?DETS_PACK_SIZE}
%% 				   }).



create_buffer(Tab,BufferXsize,BufferZsize,CoreN,Panel,Frame,Max_Zoom,Labels,Width,Zoom_Label,SchedLabels,Scarlet)->
    %% When the zoom_lvl = Max_Zoom, the whole graph is 1px.
    Zoom=Max_Zoom-trunc(math:log(Width)/math:log(2)), %%zoom_make
    refresh_buffer({0,0},
		   #buffdets{tab=Tab,
			     offset={Zoom-(BufferZsize div 2),
				     -(BufferXsize div 2)*?DETS_PACK_SIZE+1},
			     mode=ready,
			     static={BufferXsize,BufferZsize,CoreN},
			     width=Width,
			     panel=Panel,
			     frame=Frame,
			     left_data=0,
			     right_data=Width,
			     zoomin_data=Zoom,
			     zoomout_data=Max_Zoom-Zoom,
			     pos={1+BufferZsize div 2,
				  (1+(BufferXsize div 2))*?DETS_PACK_SIZE},
			     labels=Labels,
			     zoom_label=Zoom_Label,
			     max_zoom=Max_Zoom,
			     schedlabels=SchedLabels,
			     scarlet=Scarlet
			    }).	

refresh_buffer({Zadj,Xadj},Old)->
    {BufferXsize,BufferZsize,_CoreN} = Old#buffdets.static,
    {Zoffset,Xoffset} = Old#buffdets.offset,
    {Z,X} = Old#buffdets.pos,
    ZoomStart = Zoffset+Zadj,
    ZoomEnd = ZoomStart+BufferZsize,
    XStart = (Xoffset+Xadj) div ?DETS_PACK_SIZE,
    XEnd = XStart+BufferXsize,
    Data = get_from_dets(ZoomStart,ZoomEnd,XStart,XEnd,
			 Old#buffdets.tab),
    Old#buffdets{data=Data,
		 lz=2,
		 uz=BufferZsize-1,
		 lx=1+?DETS_PACK_SIZE,
		 ux=(BufferXsize-1)*?DETS_PACK_SIZE,
		 offset={Zoffset+Zadj,Xoffset+Xadj},
		 pos={Z-Zadj,X-Xadj}
		}.

get_from_dets(ZoomStart,ZoomEnd,XStart,XEnd,Tabs)->
    lists:map(fun(Zoom)->
      lists:map(fun(Tab)->
	lists:map(fun(X)->
			  cets:lookup(Tab,{Zoom,X})
	  %% case cets:lookup(Tab,{Zoom,X}) of
	  %%     [[Values]] ->Values;
	  %%     [] ->[]
	  %% end
	end, lists:seq(XStart,XEnd))
       end,Tabs)
     end,lists:seq(ZoomStart,ZoomEnd)).

category(Datapack) ->
    {ZoomLvl,Xpos} = Datapack#buffdets.pos,
    Width = Datapack#buffdets.width,
    if ZoomLvl>Datapack#buffdets.uz->
	    {1,0};
       ZoomLvl<Datapack#buffdets.lz ->
	    {-1,0};
       Xpos<Datapack#buffdets.lx ->
	    {0,-?DETS_PACK_SIZE};
       Xpos+Width>Datapack#buffdets.ux ->
	    {0,?DETS_PACK_SIZE};
       true ->
	    nu
    end.
