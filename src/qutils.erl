-module(qutils).
-compile(export_all).


select_tail(L) ->
    select_tail(L,[]).
select_tail([T],L)->
    {L,T};
select_tail([H|T],L)->
    select_tail(T,[H|L]).

take(N,L)->
    take(N,L,[]).
take(0,Rest,T)->
    {lists:reverse(T),ok,Rest};
take(M,[],T) ->
    {lists:reverse(T),M};
take(N,[H|T],Acc) ->
    take(N-1,T,[H|Acc]).

take_from_tail(N,L)->
    RL = lists:reverse(L),
    case take(N,RL) of
	{T,ok,Rest}->
	    {lists:reverse(T),ok,lists:reverse(Rest)};
	{T,M} ->
	    {lists:reverse(T),M}
    end.
		
% lists:sublist(L,P,D) will fail if P > lenght(L) + 1
% instead, we want it to return an empty list

sublist(List,Index,Len)->
		if length(List) > Index ->
						lists:sublist(List,Index,Len);
			 true ->
				lists:map(fun(_)->0 end,lists:seq(1,Len))
		end.

zip3(A,B)->
	lists:zip3(
	  lists:sublist(A,length(B)),
	  lists:sublist(B,length(A)),
	  lists:reverse(lists:seq(min(length(A),length(B))-1,0,-1))
	 ).

reregister(Name,PID)->
	case lists:member(Name,erlang:registered()) of
		true->
			unregister(Name);
		false ->
			ok
	end,
	register(Name,PID).


maptrunc(L)->
    lists:map(fun(X)->
		      strunc(X)
	      end,L).

strunc(X) when (X<1) and (X>0) ->
    1;
strunc(X) ->
    trunc(X).
