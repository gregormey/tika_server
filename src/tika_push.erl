-module(tika_push).
-export([send_invite/2]).

-include("records.hrl").

send_invite(User=#user{pushToken=PushToken},EventTitle) when PushToken =/= undefined  ->
		Method = post,
		Type = "application/json",
		URL = get_push_uri(),
		Header = get_auth_header(),
		erlang:display("Send Push:"),
		erlang:display(User#user.displayName), 
		erlang:display(PushToken), 
		Body = jiffy:encode(get_body(PushToken, 
										tika_user:batch_number(User), 
										get_invite_text(User#user.displayName,EventTitle)
									)
							),
		HTTPOptions = [],
		Options = [],
		httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options);
send_invite(_,_) -> false.

get_invite_text(CreatorName, EventTitle)->
	CreatorName++" hat dich zu "++EventTitle++" eingeladen.".

get_auth_header()->
	[
		{"X-Ionic-Application-Id","9c377b3a"},
		{"Authorization","Basic MzEzMGY5ZjJmNDM4NDkzMjYzNjMwYmQ4MDhlNzBhMjUxODhkM2EyNjNlZTc0YjQ5"}
	].

get_push_uri()->
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

