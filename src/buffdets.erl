-module(buffdets).
-compile(export_all).
-define(DETS_PACK_SIZE,1000).
-define(DEF_BUFFER_X,6).
-define(DEF_BUFFER_Z,4).

-record(buffdets, {data,
		   tab}).
%  [ zoom levels [cores [ data [values]]]]] 

open(Filename)->
    open(Filename,?DEF_BUFFER_X,?DEF_BUFFER_Z).

open(Filename,BufferXsize,BufferZsize)->
    {ok,Tab} = dets:open_file(Filename,[{access,read}]),
    [{init_state,Max_Zoom,CoreN}]=dets:lookup(Tab,init_state),
    Zoom=Max_Zoom,
    #buffdets{data=lists:map(fun(Z)->
				     getZoomLevel(Z,CoreN,Tab,BufferXsize,1)
			     end,lists:seq(Zoom,Zoom-BufferZsize,-1)),
	      tab=Tab}.

getZoomLevel(Z,CoreN,Tab,BufferXsize,Xpos)->
    lists:map(fun(CoreID)->
		  lists:map(fun(Key)->
			    case dets:match(Tab,{{CoreID,Z,Key},'_','$1'}) of
				[[Values]] -> Values;
				[] -> []
			    end
	          end, lists:seq(Xpos,Xpos+BufferXsize))
	      end,lists:seq(1,CoreN)).

getData(From,Duration,ZoomLvl,Datapack)->
    Data = Datapack#buffdets.data,
    ArrayID = (From div ?DETS_PACK_SIZE)+1,          % 1-indexed
    ArrayIndex = (From rem ?DETS_PACK_SIZE)+1,
    if ArrayIndex+Duration =< ?DETS_PACK_SIZE ->
	    lists:map(fun(CoreData)->
			      lists:sublist(lists:nth(ArrayID,CoreData),ArrayIndex,Duration)
		      end,lists:nth(ZoomLvl,Data));
        ArrayIndex+Duration > ?DETS_PACK_SIZE ->
	    lists:map(fun(CoreData)->
			      lists:append(
				lists:sublist(lists:nth(ArrayID,CoreData),ArrayIndex,?DETS_PACK_SIZE-ArrayIndex),
				lists:sublist(lists:nth(ArrayID+1,CoreData),1,Duration-?DETS_PACK_SIZE+ArrayIndex))
		      end,lists:nth(ZoomLvl,Data))
    end.
	      
