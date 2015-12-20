-module(tika_rest).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([terminate/4]).



init(_Route, _Req, State) ->
    {ok, State}.

get("/verify/:code", Req, State) ->
	Code=binary_to_list(leptus_req:param(Req,code)),
	case tika_verification:verify(Code) of 
   		not_found-> {404, {json, [{<<"Msg">>,<<"Code not found">>}]}, State};
   		ok -> {200, {json, [{<<"Msg">>,<<"Code verified">>}]}, State};
   		_ -> {500, {json, [{<<"Msg">>,<<"Internal Server Error">>}]}, State}
   	end;

get("/event/", Req, State) ->
	{200, {json, 
			[tika_event:event2json(inc_dates,X) || X <- tika_event:list() ] 
	}, State};

get("/user/", Req, State) ->
	{200, {json, 
			[tika_user:user2json(inc_dates,X) || X <- tika_user:list(with_mail) ] 
	}, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.