-module(cets).

-export([insert/2, lookup/2, decode/1, zoom_out/1]).

-export_type([seven_bit_list/0]).
-define(MAX_INT, 255).

-type key() :: {non_neg_integer(), pos_integer()}.
-type seven_bit_list() :: [0..127].

-spec insert(dets:tab_name(), {key(), seven_bit_list()}) -> 'ok' | {'error', _}.
insert(D, {K, List}) ->
    dets:insert(D, {K, encode(List)}).

-spec lookup(dets:tab_name(), key()) -> seven_bit_list().
lookup(D, K) ->
    case dets:lookup(D,K) of
	[{_,Enc}] -> decode(Enc);
	[] -> []
    end.

%% decodeS(Bin)->
%%     decodeS(Bin,0,42).

%% decodeS(<<0:8>>,_,_)->[];
%% decodeS(Bin,0,_)->
%%     <<B:1,H:7,T/binary>> = Bin,
%%     if B==0 ->
%% 	    [H|decodeS(T)];
%%        B==1->
%% 	    <<N:8,T2/binary>>=T,
%% 	    decodeS(T2,N,H)

%%     end;
%% decodeS(Bin,N,X)->
%%     [X|decodeS(Bin,N-1,X)].

-spec decode(binary()) -> seven_bit_list().
decode(Bin) ->
    decode_(binary:bin_to_list(Bin)).

decode_([0]) -> [];
decode_([H1|T])->
    <<B:1,H:7>> = <<H1:8>>,
    case B of
	0 ->
	    [H|decode_(T)];
	1 ->
	    [N|T2] = T,
	    lists:duplicate(N, H) ++ decode_(T2)
    end.

%% encode1([])->
%%     <<0:8>>;
%% encode1(L)->
%%     case get_same(L) of
%% 	{T,1,H}->
%% 	    <<0:1,H:7, (encode1(T))/binary>>;
%% 	{T,N,H} ->
%% 	    <<1:1,H:7,N:8, (encode1(T))/binary>>
%%    end.

encode(X) ->
%    encode1(X).
    binary:list_to_bin(encode_(X)).

encode_([]) ->
    [<<0:8>>];
encode_(L)->
    case get_same(L) of
	{T,1,H} ->
	    [<<0:1,H:7>>|encode_(T)];
	{T,N,H} ->
	    [<<1:1,H:7>>, <<N:8>>|encode_(T)]
    end.

%% encode2([])->
%%     [0];
%% encode2(L)->
%%     case get_same(L) of
%% 	{T,1,H}->
%% 	    [H|encode2(T)];
%% 	{T,N,H} ->
%% 	    [H+128,N|encode(T)]
%%     end.

%% get_same2([H|T])->
%%     {A,B}=lists:splitwith(fun(X)->
%% 			    X==H
%% 			  end,T),
%%     {B,length([H|A]),H}.

get_same(L) ->
    get_same(L, 0).

get_same([H|T], ?MAX_INT) ->
    {[H|T], ?MAX_INT, H};
get_same([H],A) ->
    {[],(A+1),H};
get_same([H,H|T],A) ->
    get_same([H|T],A+1);
get_same([H1,H2|T],A) ->
    {[H2|T],A+1,H1}.

%% zoom_out([H1,H2|T])->
%%     [trunc((H1+H2)/2)|zoom_out(T)];
%% zoom_out([H]) ->
%%     [trunc(H/2)];
%% zoom_out([]) ->
%%     [].

-spec zoom_out(binary()) -> <<_:8,_:_*8>>.
zoom_out(Bin) ->
    zo(binary:bin_to_list(Bin)).

zo([0]) ->
    <<0>>;
zo([H,0])->
    zo([H,0,0]);
zo([H1,H2|T])->
    <<B:1,H:7>> = <<H1:8>>,
    case B of
	0 ->
	    <<B2:1,H22:7>> = <<H2:8>>,
	    <<0:1,((H+H22) div 2):7,
	      case B2 of
		  0 ->
		      zo(T);
		  1 ->
		      [N|TT]=T,
		      zoN(H22,N-1,TT)
	      end/binary>>;
	1 ->
	    zoN(H,H2,T)
    end.

zoN(_,0,T)->
    zo(T);
zoN(H,1,T) ->
    zo([H|T]);
zoN(H,N,T) ->
    <<HN:7,R:1>> = <<N:8>>,
    <<1:1,H:7,HN:8,case R of
		       1 ->
			   zo([H|T]);
		       0 ->
			   zo(T)
		   end/binary>>.

%%----------------------- Tests below ------------------------------

-ifdef(TEST).

test(N)->
    Keys = lists:seq(1,250*N),
    L = [X div 10 || X <- lists:seq(1, 4096)],
    {ok, T} = dets:open_file(lala, []),
    DL = encode(L),
    A = erlang:now(),
    lists:foreach(fun(X) -> dets:insert(T,{X,L}) end, Keys),
    B = erlang:now(),
    lists:foreach(fun(_) -> encode(L) end, Keys),
    C = erlang:now(),
    lists:foreach(fun(X) -> dets:insert(T,{X,DL}) end, Keys),
    D = erlang:now(),
    dets:close(T),
    {ok,TT} = dets:open_file(lala, []),
    DD = erlang:now(),
    lists:foreach(fun(X) -> dets:lookup(TT,X) end, Keys),
    E = erlang:now(),
    lists:foreach(fun(X) -> dets:lookup(TT,X) end, Keys),
    F = erlang:now(),
    lists:foreach(fun(_) -> zoom_out(DL) end, Keys),
    G = erlang:now(),
    lists:foreach(fun(_) -> decode(DL) end, Keys),
    H = erlang:now(),
    io:write({t,
	      timer:now_diff(B,A) div 10000,  %ins
	      timer:now_diff(C,B) div 10000,  %enc
	      timer:now_diff(D,C) div 10000,  %insec
	      timer:now_diff(E,DD) div 10000, %look
	      timer:now_diff(F,E) div 10000,  %lookenc
	      timer:now_diff(G,F) div 10000,  %zo
	      timer:now_diff(H,G) div 10000   %zoomout
	     }),
    io:nl().

-endif.
