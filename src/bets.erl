-module(bets).

-export([open_file/3, lookup/2, lookup_otr/2, plain_lookup_otr/2, close/1]).

-record(bets, {tab,
               centre,
               buffer = [],
               update = false,
               id}).

-opaque bets() :: #bets{}.
-type key()    :: {non_neg_integer(), integer()}.

-spec open_file(file:filename(), [{access,dets:access()}], ibap:core_id()) ->
                       bets().                       
open_file(FileName, Options, ID) ->
    {ok, Tab} = dets:open_file(FileName,Options),
    #bets{tab = Tab,
          id = ID}.

-spec plain_lookup_otr(bets(), key()) -> cets:seven_bit_list().                              
plain_lookup_otr(Bets,Key)->
    dets:lookup(Bets#bets.tab,Key).

-spec lookup_otr(bets(), key()) -> cets:seven_bit_list().                         
lookup_otr(Bets, Key) ->
    case lists:keyfind(Key, 1, Bets#bets.buffer) of
        false -> cets:lookup(Bets#bets.tab, Key);
        {Key, Value} -> Value
    end.

-spec lookup(bets(),key()) -> {bets(), cets:seven_bit_list()}.                    
lookup(Bets, Key)->
    ID = Bets#bets.id,
    NBets = if Bets#bets.update -> 
                    receive 
                        {updated_bets, ID, NewBets} -> NewBets
                    end;
               true -> Bets
            end,
    Data = lookup_otr(NBets, Key),
    RBets = update_check(NBets,Key,Data),
    {RBets,Data}.

update_check(Bets,Key,Data)->
    if Key == Bets#bets.centre -> Bets;
       true -> 
            Self = self(),
            UBets = Bets#bets{buffer = [{Key,Data},Bets#bets.buffer]},
            spawn(fun() -> update(Self,UBets,Key) end),
            Bets#bets{update = true}
    end.

update(PID,Bets,{ZKey,XKey})->   
    O = [-1,0,1],
    Buffer = [{{ZKey+OZ,XKey+OX}, 
               bets:lookup_otr(Bets,{ZKey+OZ,XKey+OX})} || OX <- O, OZ <- O],

    PID ! {updated_bets, Bets#bets.id, Bets#bets{ buffer = Buffer,
                                                  centre = {ZKey, XKey},
                                                  update = false}}.

-spec close(bets) -> 'ok' | {'error', _}.
close(Bets)->
    dets:close(Bets#bets.tab).
    
