-module(dt).
-compile(export_all).

dt()->
    {ok,S}=file:open(qijap_trace,[write,raw,compressed]),
    bwriteln(S,2),
    lists:map(fun(P)->
		      bwriteln(S,P)
	      end, [
		    [
		     {1,{1,in,m,0}},
		     {1,{1,out,m,2}},
		     {1,{1,in,m,4}},
		     {1,{1,out,m,8}},
		     {1,{1,in,m,7}},
		     {1,{1,out,m,5}}
		    ],
		    [
		     {2,{1,in,m,10}},
		     {2,{1,out,m,12}},
		     {1,{1,in,m,14}},
		     {2,{1,out,m,18}},
		     {2,{1,in,m,17}},
		     {1,{1,out,m,15}}
		    ]]),
    file:close(S).
		    




bwriteln(F,P)->
    file:write(F,binary:list_to_bin([term_to_binary(P),10])).
				   
