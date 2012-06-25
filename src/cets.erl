-module(cets).
-compile(export_all).

-define(MAX_INT,255).

insert(D,{K,List})->
    dets:insert(D,{K,encode(List)}).

lookup(D,K)->
    case dets:lookup(D,K) of
	[{_,Enc}] ->
	    decode(Enc);
	[] ->[]
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

decode(Bin)->
    decode_(binary:bin_to_list(Bin)).

decode_([0])->[];
decode_([H1|T])->
    <<B:1,H:7>> = <<H1:8>>,
    if B==0 ->
    	    [H|decode_(T)];
       B==1->
	    [N|T2]=T,
	    lists:append(lists:duplicate(N,H),decode_(T2))
    end.

encode([])-><<0:8>>;
encode(L)->
    case get_same(L) of
	{T,1,H}->
						%	    <<H:8, (encode(T))/binary>>;
	    <<0:1,H:7, (encode(T))/binary>>;
	{T,N,H} ->
						%	    <<H:8,N:8, (encode(T))/binary>>
	    <<1:1,H:7,N:8, (encode(T))/binary>>
    end.


get_same(L)->
    get_same(L,0).
get_same([H|T],?MAX_INT)->
    {[H|T],?MAX_INT,H};
get_same([H],A)->
    {[],(A+1),H};
get_same([H,H|T],A)->
    get_same([H|T],A+1);
get_same([H1,H2|T],A)->
    {[H2|T],A+1,H1}.


test(N)->
    Keys = lists:seq(1,250*N),
    L=lists:map(fun(X)->
			X div 10
		end,lists:seq(1,4096)),
    {ok,T}=dets:open_file(lala,[]),
    DL=encode(L),
    A=erlang:now(),
    lists:map(fun(X)->
		      dets:insert(T,{X,L})
	      end,Keys),
    B=erlang:now(),

    lists:map(fun(_)->
		      encode(L)
	      end,Keys),
    C=erlang:now(),

    lists:map(fun(X)->
		      dets:insert(T,{X,DL})
	      end,Keys),

    D=erlang:now(),
    dets:close(T),
    {ok,TT}=dets:open_file(lala,[]),
    DD=erlang:now(),
    lists:map(fun(X)->
		      dets:lookup(TT,X)
	      end,Keys),
    E=erlang:now(),

    lists:map(fun(X)->
		      dets:lookup(TT,X)
	      end,Keys),
    F=erlang:now(),
    lists:map(fun(_)->
		      zoom_out(DL)
	      end,Keys),
    G=erlang:now(),
    lists:map(fun(_)->
		      decode(DL)
						%		      zoom_out(L)
	      end,Keys),
    H=erlang:now(),
    io:write({t,
	      timer:now_diff(B,A) div 10000,  %ins
	      timer:now_diff(C,B) div 10000,  %enc
	      timer:now_diff(D,C) div 10000,  %insec
	      timer:now_diff(E,DD) div 10000,  %look
	      timer:now_diff(F,E) div 10000,  %lookenc
	      timer:now_diff(G,F) div 10000,   %zo
	      timer:now_diff(H,G) div 10000   %zoomout
	     }),
    io:nl().



%% zoom_out([H1,H2|T])->
%%     [trunc((H1+H2)/2)|zoom_out(T)];
%% zoom_out([H]) ->
%%     [trunc(H/2)];
%% zoom_out([]) ->
%%     [].

zoom_out(L)->
    zo(binary:bin_to_list(L)).

zo([0])->
    <<0>>;
zo([H,0])->
    zo([H,0,0]);
zo([H1,H2|T])->
    <<B:1,H:7>> = <<H1:8>>,
    if B==0 ->
	    <<B2:1,H22:7>> = <<H2:8>>,
	    <<0:1,((H+H22) div 2):7,
	      (if B2==0 ->
		       zo(T);
		  B2==1->
		       [N|TT]=T,
		       zoN(H22,N-1,TT)
	       end)/binary>>;

       B==1->
	    zoN(H,H2,T)
    end.

zoN(_,0,T)->
    zo(T);
zoN(H,1,T) ->
    zo([H|T]);
zoN(H,N,T) ->
    <<HN:7,R:1>> = <<N:8>>,
    <<1:1,H:7,HN:8,( if R==1 ->
			     zo([H|T]);
			R==0 ->
			     zo(T)
		     end)/binary>>.

