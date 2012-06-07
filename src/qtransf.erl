-module(qtransf).
-compile(export_all).
-define(DEF_IN,qijap_trace).
-define(DEF_OUT,dets_trace).
-define(MAX,42).


% returns a list of tupples to sort or end_of_data

get_pack(Fin)->
    case file:read_line(Fin) of
	eof -> 
	    end_of_data;
	{ok,B} ->
	    binary_to_term(B)
    end.

open_in(NameIn)->
    {ok,Fin}=file:open(NameIn,[read,raw,binary,compressed,read_ahead]),
    Fin.

open_out(NameOut)->
    {ok,Fout} = dets:open_file(NameOut,[]),
    Fout.

close_in(Fin)->
    file:close(Fin).

close_out(Fout)->
    dets:close(Fout).

shedn(Fin,Fout)->
    {ok,In} = file:read_line(Fin),
    N=binary_to_term(In),
    dets:insert(Fout,{shedn,N}),
    N.

compare({_,_,_,T1},{_,_,_,T2})->
    if T1>=T2 ->
	    bigger;
       T1<T2 ->
	    smaller
    end.

store(_,_,[])->ok;
store(Fout,SID,[{PID,out,MFAout,Tout},{PID,in,MFAin,Tin}|Ps])->
    dets:insert(Fout,{SID,Tin,Tout,PID,{MFAin,MFAout}}),
    store(Fout,SID,Ps).
		

loop()->
    loop(?DEF_IN,?DEF_OUT).
loop(NameIn,NameOut)->
    Fin = open_in(NameIn),
    Fout = open_out(NameOut),
    ShedN = shedn(Fin,Fout),
    PIDs=lists:map(fun(SID)->
			   spawn(fun()->
					 insert_node(Fout,SID,[],1)
				 end)
		   end,lists:seq(1,ShedN)),
    loop(Fin,Fout,PIDs).
	    

loop(Fin,Fout,PIDs)->
    case get_pack(Fin) of
	end_of_data -> 
	    lists:map(fun(PID)-> PID!{exit,self()} end,PIDs),
	    lists:map(fun(_PID)->
			      receive
				  done-> ok
			      end
		      end, PIDs),
	    close_in(Fin),
	    close_out(Fout);
		
	P ->
	    insert_pack(P,PIDs),
	    loop(Fin,Fout,PIDs)
    end.
    
    

insert_pack([],_)->ok;
insert_pack([{SID,P}|Ps],PIDs)->
    PID=lists:nth(SID,PIDs),
    PID!P,
    insert_pack(Ps,PIDs).
   
 
    
insert(X,[])->[X];
insert(X,[S|Ss])->
    case compare(X,S) of
	bigger ->
	    [X,S|Ss];
	smaller ->
	    [S|insert(X,Ss)]
    end.



insert_node(Fout,SID,S,?MAX)->
    store(Fout,SID,S),
    insert_node(Fout,SID,[],1);
insert_node(Fout,SID,S,N)->
    receive
	{exit,PID}->
	    store(Fout,SID,S),
	    PID!done;
	P ->
	    SN=insert(P,S),
	    insert_node(Fout,SID,SN,N+1)
    end.


bwriteln(F,P)->
    file:write(F,binary:list_to_bin([term_to_binary(P),10])).
