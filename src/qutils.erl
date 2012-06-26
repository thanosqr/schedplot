-module(qutils).

-export([ceiling/1, sublist/3, zip3/2, reregister/2, maptrunc/1,
	 strunc/1, round/2]).

-spec ceiling(number()) -> integer().
ceiling(X) ->
    T = trunc(X),
    if T == X ->
	    T;
       true ->
	    T+1
    end.

%% select_tail(L) ->
%%     select_tail(L,[]).

%% select_tail([T],L) ->
%%     {L,T};
%% select_tail([H|T],L) ->
%%     select_tail(T,[H|L]).

%% take(N,L) ->
%%     take(N,L,[]).

%% take(0,Rest,T) ->
%%     {lists:reverse(T),ok,Rest};
%% take(M,[],T) ->
%%     {lists:reverse(T),M};
%% take(N,[H|T],Acc) ->
%%     take(N-1,T,[H|Acc]).

%% take_from_tail(N,L) ->
%%     RL = lists:reverse(L),
%%     case take(N,RL) of
%% 	{T,ok,Rest} ->
%% 	    {lists:reverse(T),ok,lists:reverse(Rest)};
%% 	{T,M} ->
%% 	    {lists:reverse(T),M}
%%     end.

		
%% lists:sublist(L,P,D) will fail if P > length(L) + 1
%% instead, we want it to return an empty list

-spec sublist(list(), non_neg_integer(), non_neg_integer()) -> list().
sublist(List, Index, Len) ->
    if length(List) > Index ->
	    lists:sublist(List, Index, Len);
       true ->
	    lists:duplicate(Len, 0)
    end.

-spec zip3([A], [B]) -> [{A,B,integer()}].
zip3(A, B) ->
    lists:zip3(
      lists:sublist(A,length(B)),
      lists:sublist(B,length(A)),
      lists:reverse(lists:seq(min(length(A),length(B))-1,0,-1))
     ).

-spec reregister(atom(), pid()) -> 'ok'.			
reregister(Name, PID) ->
    case lists:member(Name, erlang:registered()) of
	true ->
	    unregister(Name);
	false ->
	    ok
    end,
    true = register(Name, PID),
    ok.

-spec maptrunc([float()]) -> [non_neg_integer()].
maptrunc(L) ->
    [strunc(X) || X <- L].

-spec strunc(float()) -> non_neg_integer().
strunc(X) when 0 < X, X < 1 ->
    1;
strunc(X) when 0 =< X ->
    trunc(X).

-spec round(float(), non_neg_integer()) -> float().
round(F, P) ->
    R = math:pow(10, P),
    round(F*R)/R.
