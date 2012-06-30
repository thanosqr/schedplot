-module(mg).

-compile(export_all).

gen_n(MsgLength, MsgInterval, MsgN, PID, CoreN, Wait) ->
    _ = [spawn( fun() ->
                        gen(MsgLength, MsgInterval, MsgN, 
                            PID, Core, Wait)
                end) || Core <- lists:seq(1,CoreN)].

gen(MsgLength, MsgInterval, MsgN, PID, Core, Wait) ->
    gen({0,0,0}, MsgLength, MsgInterval, MsgN, PID, Core, Wait).

gen(_TimeIn, _MsgLength, _MsgInterval, 0, _PID, _Core, _Wait) -> ok;
gen(TimeIn, MsgLength, MsgInterval, MsgN, PID, Core, Wait) ->
    PID ! {trace_ts,PID,in,Core,{lists,seq,2},TimeIn},
    TimeOut = time_add(TimeIn,MsgLength),
    wait(Wait,TimeOut,TimeIn),
    PID ! {trace_ts,PID,out,Core,{lists,seq,2},TimeOut},
    gen(time_add(TimeOut,MsgInterval), MsgLength, 
        MsgInterval,MsgN-1,PID,Core,Wait).



time_add({Q,W,E},{A,S,D})->
    Z = Q+A,
    X = W+S,
    C = E+D,
    {CC,XX} = if C < 1000000 ->
                      {C,X};
                 true ->
                      {C-1000000,X+1}
              end,
    {ZZ,XXX} = if XX < 1000000->
                       {Z,XX};
                  true ->
                       {Z+1,XX-1000000}
               end,
    {ZZ,XXX,CC}.


            
       
    


wait(no, _, _)-> ok;
wait(calc, T2, T1) -> 
    timer:sleep(round(timer:now_diff(T2,T1)/1000000)).
