-module(famdict).
-compile(export_all).
-define(UD,dict).
-define(MAX_DICT_SIZE,255).
-define(MAX_TOTAL_SIZE,420).
-define(MAX_MODULE_SIZE,15).

-record(famdict, {current,
		  old=[],
		  size=0,
		  file,
		  total_size=1}).
		  
	  
%{ {F,A}, {ID1,[{M,ID2}]} }

check({M,F,A},D)->
    check({F,A},M,D).         %MOPT {F,A}
check(FA,M,D)->
    case flookup(FA,D) of
	not_found ->
	    {DN,ID1,ID2} = add_function(FA,M,D);
	{ID1t,Modules}->
	    case mlookup(M,Modules) of
		not_found -> 
		    {DN,ID1,ID2} = add_module(FA,M,D,length(Modules),ID1t);
		ID2 ->
		    DN=D,			
		    ID1=ID1t
	    end
    end,
    {{ID1,ID2},DN}.


new(Name)->
    {ok,F}=file:open(Name,[raw,write]),
    #famdict{current=?UD:new(),file=F}.
		 
add_function(FA,M,D)->
io:write('-------'),io:nl(),
    case D#famdict.size of
	?MAX_DICT_SIZE ->
	    DN = add_slot(D),
	    S=0;
        S ->
	    DN = D
    end,
    FD = ?UD:store(FA,{S+1,[M]},D#famdict.current),
    {DN#famdict{current=FD, size=S+1},S+1,0}.

add_module(FA,M,D,?MAX_MODULE_SIZE,_)->
    add_function(FA,M,add_slot(D));
add_module(FA,M,D,N,ID1) ->
    DN = ?UD:update(FA,fun({ID,Ms})->
			       {ID,[M|Ms]}
		       end,D#famdict.current),
    {D#famdict{current=DN},ID1,N+1}.
	
    
add_slot(D)->
    case D#famdict.total_size of 
	?MAX_TOTAL_SIZE ->
	    save(D#famdict.old,D#famdict.file),
	    D#famdict{current=?UD:new(),
		      size=0,
		      total_size=1,
		      old=[]};
	N ->
	    D#famdict{current=?UD:new(),
		      size=0,
		      total_size=N+1,
		      old=[D#famdict.current|D#famdict.old]}
    end.
		      
% TOPT
save(D,F)->
    file:write(F,term_to_binary(D)).

flookup(FA,D)->
    case ?UD:find(FA,D#famdict.current) of
	{ok,Value}->
	    Value;
	error ->
	    not_found
    end.

mlookup(M,Ms)->
    mlookup(M,Ms,length(Ms)).
mlookup(_,[],_)->
    not_found;
mlookup(M,[M|_],N)->
    N;
mlookup(M,[_|Ms],N) ->
    mlookup(M,Ms,N-1).
    
close(D)->
    save([D#famdict.current|D#famdict.old],D#famdict.file),
    file:close(D#famdict.file).
