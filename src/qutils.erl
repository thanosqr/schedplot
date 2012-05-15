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
				io:write({Index,Len}),
				lists:map(fun(_)->0 end,lists:seq(1,Len))
		end.
