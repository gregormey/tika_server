-module(tika_verification).
-behaviour(gen_server).


%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% custom interfaces
-export([create_verification/1]).
-export([verify/1]).
-export([list/0]).
-export([load/2]).
-export([remove/2]).

%% default interfaces
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-include("records.hrl").
-define(DEFAULT_VERIFICATION_BASE_URL, "http://localhost:8080/#/verify/").


-type verification() :: #verification {}.


-spec start() -> {ok, pid()} | {error, any()}.
start()-> gen_server:start_link({local,?MODULE},?MODULE,[], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop()-> gen_server:call(?MODULE, stop).


-spec create_verification(string()) -> tuple().
create_verification(Mail) -> gen_server:call(?MODULE,{create_verification,Mail}).

-spec verify(string()) -> tuple() | not_found.
verify(Code)-> gen_server:call(?MODULE,{verify,Code}).

-spec list() -> list().
list()-> gen_server:call(?MODULE,list).

-spec load(mail, string()) -> verification().
load(mail,Mail)-> gen_server:call(?MODULE,{load,mail,Mail}).
	
-spec remove(mail, string()) -> verification().
remove(mail,Mail)-> gen_server:call(?MODULE,{remove,mail,Mail}).
	


%% gen_server
init([]) ->
    {ok, ?MODULE}.


handle_call({create_verification,Mail}, _From, Tab) ->
	[Verification]=tika_database:write(verification,
						#verification{
							mail=Mail,
							created=tika_database:unixTS(),
							code=get_verification_code(Mail)
						}),
	User=tika_user:load(mail,Verification#verification.mail), 
	{reply, tika_mail:send_verification(User#user.mail,
						User#user.displayName,
						tika_config:get_value(config,[config,verification_base_url], ?DEFAULT_VERIFICATION_BASE_URL)
						++Verification#verification.code), Tab};

handle_call({verify,Code}, _From, Tab) ->
	Fun=fun(R) ->
				Code==R#verification.code
		end,
	{reply,
		case tika_database:find(verification,Fun) of
			not_found -> not_found;
			[Verification] -> tika_database:write(verification,
							Verification#verification{
								verified=tika_database:unixTS()
							}),
							ok
		end
	,Tab};


handle_call(list, _From, Tab) ->
	{reply,
		tika_database:find(verification)
	,Tab};

handle_call({load,mail,Mail}, _From, Tab) ->
	Fun=fun(R) ->
					Mail==R#verification.mail
		end,
	{reply,
		case tika_database:find(verification,Fun) of
			[Verification] -> Verification;
			not_found -> not_found
		end
	,Tab};

handle_call({remove,mail,Mail}, _From, Tab) ->
	{reply,
		tika_database:delete(verification,#verification{mail=Mail})
	,Tab};

handle_call(stop, _From, Tab) ->
	{stop, normal, stopped, Tab}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%internal.
get_verification_code(Mail)->
	hmac:hexlify(hmac:hmac256(integer_to_binary(tika_database:unixTS()),
								list_to_binary(Mail))).

