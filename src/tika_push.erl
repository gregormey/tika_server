-module(tika_push).
-export([send_invite/2]).

-include("records.hrl").

send_invite(User=#user{},EventTitle) ->
	Method = post,
	Type = "application/json",
	
	URL = get_push_url(),
	Header = get_auth_header(),
	Body = jiffy:encode(get_body(User#user.pushToken, 
									tika_user:batch_number(User), 
									get_invite_text(User#user.displayName,EventTitle)
								)
						),
	HTTPOptions = [],
	Options = [],
	httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options).

get_invite_text(CreatorName, EventTitle)->
	CreatorName++" hat dich zu "++EventTitle++" eingeladen.".

get_auth_header()->
	[
		{"X-Ionic-Application-Id","9c377b3a"},
		{"Authorization","Basic MzEzMGY5ZjJmNDM4NDkzMjYzNjMwYmQ4MDhlNzBhMjUxODhkM2EyNjNlZTc0YjQ5"}
	].

get_push_url()->
	"https://push.ionic.io/api/v1/push".

get_body(Token, Badge, Message)->
{[
	{<<"tokens">>,[list_to_binary(Token)]},
	{<<"notification">>,
		{[
			{<<"alert">>, list_to_binary(Message)},
			{<<"ios">>,
				{[
					{<<"badge">>,Badge},
					{<<"sound">>,<<"ping.aiff">>},
      				{<<"expiry">>, 1423238641},
      				{<<"priority">>, 10},
      				{<<"contentAvailable">>,true},
      				{<<"payload">>,
      					{[
      						{<<"key1">>,<<"value">>},
        					{<<"key2">>,<<"value">>}
      					]}
      				}
				]}
			},
			{<<"android">>,
				{[
					{<<"collapseKey">>,<<"foo">>},
					{<<"delayWhileIdle">>,true},
      				{<<"timeToLive">>, 300},
      				{<<"payload">>,
      					{[
      						{<<"key1">>,<<"value">>},
        					{<<"key2">>,<<"value">>}
      					]}
      				}
				]}
			}
		]}
	}
]}.

