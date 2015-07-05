-module(tika_push).
-export([send_push/3]).

send_push(Token, Badge, Message) ->
	Method = post,
	URL = "https://push.ionic.io/api/v1/push",
	Header = [
				{"X-Ionic-Application-Id","9c377b3a"},
				{"Authorization","Basic MzEzMGY5ZjJmNDM4NDkzMjYzNjMwYmQ4MDhlNzBhMjUxODhkM2EyNjNlZTc0YjQ5"}
			],
	Type = "application/json",
	Body = jiffy:encode(get_body(Token, Badge, Message)),
	HTTPOptions = [],
	Options = [],
	httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options).


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

