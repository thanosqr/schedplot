-module(buffdets).
-compile(export_all).
-include("hijap.hrl").

-define(DEF_BUFFER_X,5).
-define(DEF_BUFFER_Z,5).


%  [ zoom levels [cores [ data [values]]]]] 

open(Filename,Panel,Frame)->
    open(Filename,?DEF_BUFFER_X,?DEF_BUFFER_Z,Panel,Frame).

open(Filename,BufferXsize,BufferZsize,Panel,Frame)->
	Width=1000,
    {ok,Tab} = dets:open_file(Filename,[{access,read}]),
    [{init_state,Max_Zoom,CoreN}]=dets:lookup(Tab,init_state),
	Labels=plotter:create_labels(Frame,Width),
	Zoom_Label=wxStaticText:new(Frame,?ANY,"",[{pos,{Width-144,642}}]),
	create_buffer(Tab,BufferXsize,BufferZsize,CoreN,Panel,Frame,Max_Zoom,Labels,Width,Zoom_Label).

getData(Datapack)->
	{ZoomLvl,From} = Datapack#buffdets.pos,
	Duration = Datapack#buffdets.width,
    Data = Datapack#buffdets.data,
    ArrayID = (From div (?DETS_PACK_SIZE+1)),          % 1-indexed
    ArrayIndex = (From rem (?DETS_PACK_SIZE+1)),
    if ArrayIndex+Duration =< ?DETS_PACK_SIZE ->
	    lists:map(fun(CoreData)->
						  DL=lists:nth(ArrayID,CoreData),
						  qutils:sublist(DL,ArrayIndex,Duration)
		      end,lists:nth(ZoomLvl,Data));
        ArrayIndex+Duration > ?DETS_PACK_SIZE ->
	    lists:map(fun(CoreData)->
			      lists:append(
				qutils:sublist(lists:nth(ArrayID,CoreData),ArrayIndex,?DETS_PACK_SIZE-ArrayIndex),
				qutils:sublist(lists:nth(ArrayID+1,CoreData),1,Duration-?DETS_PACK_SIZE+ArrayIndex))
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
	{getData(Datapack),
	 case category(Datapack) of
		 nu ->
			 Datapack;
		 Adj ->
			 spawn(?MODULE,update,[self(),Adj,Datapack]),
			 Datapack#buffdets{mode=update}
	 end}.

update(PID,Adj,Old)->
	Buffer=refresh_buffer(Adj,Old),
	PID!{new_buffer,Buffer}.	




create_buffer(Tab,BufferXsize,BufferZsize,CoreN,Panel,Frame,Max_Zoom,Labels,Width,Zoom_Label)->
	Xs=10000,
	Zs = 42,
% When the zoom_lvl = Max_Zoom, the whole graph is 1px.
	Zoom=Max_Zoom-trunc(math:log(Width)/math:log(2)),
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
							 right_data=Xs*Xs,
							 zoomin_data=Zs*Zs,
							 zoomout_data=0,
							 pos={1+(BufferZsize div 2),
								  (1+(BufferXsize div 2))*?DETS_PACK_SIZE},
							 labels=Labels,
							 zoom_label=Zoom_Label,
							 max_zoom=Max_Zoom
							}).	

refresh_buffer({Zadj,Xadj},Old)->
	{BufferXsize,BufferZsize,CoreN} = Old#buffdets.static,
	{Zoffset,Xoffset} = Old#buffdets.offset,
	{Z,X} = Old#buffdets.pos,
	ZoomStart = Zoffset+Zadj,
	ZoomEnd = ZoomStart+BufferZsize,
	XStart = Xoffset+Xadj,
	XEnd = XStart+BufferXsize*?DETS_PACK_SIZE,
	Data = get_from_dets(ZoomStart,ZoomEnd,XStart,XEnd,
						 CoreN,Old#buffdets.tab),
	Old#buffdets{data=Data,
				 lz=2,
				 uz=BufferZsize-1,
				 lx=1+?DETS_PACK_SIZE,
				 ux=(BufferXsize-1)*?DETS_PACK_SIZE,
				 offset={Zoffset+Zadj,Xoffset+Xadj},
				 pos={Z-Zadj,X-Xadj}
				}.

get_from_dets(ZoomStart,ZoomEnd,XStart,XEnd,CoreN,Tab)->
		lists:map(fun(Zoom)->
		   lists:map(fun(CoreID)->
			 lists:map(fun(X)->
				case dets:match(Tab,{{CoreID,Zoom,X},'_','$1'}) of
					[[Values]] ->Values;
					[] ->[]
				end
			  end, lists:seq(XStart,XEnd,?DETS_PACK_SIZE))
			end,lists:seq(1,CoreN))
		  end,lists:seq(ZoomStart,ZoomEnd)).
			  

	
category(Datapack)->
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

