-module(tika_user).
-export([invite/2,create/0,user2json/1,json2user/1]).
-include("records.hrl").

-type user() :: #user {}.



invite(#user{mail=Mail, displayName=DisplayName},#event{title=Title}) ->
	tika_mail:send_invite(Mail,DisplayName,Title).


-spec user2json(user()) ->  tuple().
user2json(User=#user{})->
	{[
	  {<<"id">>,User#user.id},
	  {<<"displayName">>,list_to_binary(User#user.displayName)},
	  {<<"mail">>,list_to_binary(User#user.mail)}
	 ]}.

-spec json2user(tuple()) ->  user().
json2user(Json)->
	{[
	  {_,Id},
	  {_,DisplayName},
	  {_,Mail}
	 ]}=Json,

	#user{
		id=Id,
		displayName=binary_to_list(DisplayName),
		mail=binary_to_list(Mail)  
	}.

-spec create() ->  user().
create()->
	tika_database:create(user,#user{}).
	