-module(tika_push).
-export([send_invite/2]).
-export([send_confirm/4]).
-export([send_deconfirm/4]).
-export([send_reject/3]).
-export([send_fix/2]).

-include("records.hrl").

send_invite(User=#user{pushToken=PushToken},
			#event{title=EventTitle,creator=Creator}) when PushToken =/= undefined  ->
	send_notfication(User,get_invite_text(Creator#user.displayName,EventTitle));
send_invite(_,_) -> false.

send_fix([User|T],
		Event)->
		send_fix(tika_user:load(mail,User#user.mail),Event),
		send_fix(T,Event);
send_fix(User=#user{pushToken=PushToken,mail=UserMail},
		#event{title=EventTitle,appointment=#day{timestamp=DayTs},creator=#user{mail=CreatorMail}}) when 
		PushToken =/= undefined,
		UserMail =/= CreatorMail  ->
	send_notfication(User,get_fix_text(EventTitle,DayTs));
send_fix([],_) -> ok;
send_fix(_,_) -> false.

send_confirm([User|T],
			Event,
			Sender,	
			DayTs
			)->
	send_confirm(tika_user:load(mail,User#user.mail),Event,Sender,DayTs),
	send_confirm(T,Event,Sender,DayTs);
send_confirm(User=#user{pushToken=PushToken, mail=UserMail},
			#event{title=EventTitle},
			#user{displayName=DisplayName,mail=SenderMail},
			DayTs) when 
			PushToken =/= undefined,  
			UserMail =/= SenderMail ->
	send_notfication(User,get_confirm_text(DisplayName,DayTs, EventTitle));
send_confirm([],_,_,_) -> ok;
send_confirm(_,_,_,_) -> false.

	
send_deconfirm([User|T],
			Event,
			Sender,
			DayTs)->
	send_deconfirm(tika_user:load(mail,User#user.mail),Event,Sender,DayTs),
	send_deconfirm(T,Event,Sender,DayTs);
send_deconfirm(User=#user{pushToken=PushToken, mail=UserMail},
			#event{title=EventTitle},
			#user{displayName=DisplayName, mail=SenderMail},
			DayTs) when 
			PushToken =/= undefined,
			UserMail =/= SenderMail   ->
	send_notfication(User,get_deconfirm_text(DisplayName,DayTs, EventTitle));
send_deconfirm([],_,_,_) -> ok;
send_deconfirm(_,_,_,_) -> false.

send_reject([User|T],
			Rejector=#user{},
			Event=#event{})->
	send_reject(tika_user:load(mail,User#user.mail),Rejector,Event),
	send_reject(T,Rejector,Event);
send_reject(User=#user{pushToken=PushToken},
			#user{displayName=DisplayName},
			#event{title=EventTitle}) when PushToken =/= undefined  ->
	send_notfication(User,get_reject_text(DisplayName, EventTitle));
send_reject([],_,_) -> ok;
send_reject(_,_,_) -> false.

send_notfication(User=#user{pushToken=PushToken},Text)->
	Method = post,
	Type = "application/json",
	URL = get_push_uri(),
	Header = get_auth_header(),
	erlang:display("Send Push:"),
	erlang:display(User#user.displayName), 
	erlang:display(PushToken), 
	Body = jiffy:encode(get_body(PushToken, 
									tika_user:batch_number(User), 
									Text
								)
						),
	HTTPOptions = [],
	Options = [],
	erlang:display(httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options)),
	true.

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


%%text
get_invite_text(CreatorName, EventTitle)->
	CreatorName++" hat dich zu "++EventTitle++" eingeladen.".

get_confirm_text(DisplayName,DayTs, EventTitle) ->
	DisplayName ++" hat am "++msToDateString(DayTs)++" zu "++EventTitle++" Zeit.".

get_deconfirm_text(DisplayName,DayTs, EventTitle) ->
	DisplayName ++" hat am "++msToDateString(DayTs)++" zu "++EventTitle++" doch keine Zeit.".

get_reject_text(DisplayName, EventTitle) ->
	DisplayName ++ " zu "++EventTitle++" abgesagt.".

get_fix_text(EventTitle, DayTs) ->
	EventTitle ++ " findet am "++msToDateString(DayTs)++" statt.".	


%%internal
msToDateString(Milliseconds) ->
   {Year,Month,Day}=tika_database:msToDate(Milliseconds),
   YearStr=integer_to_list(Year),
   case Month < 10 of
   		true -> MonthStr="0"++integer_to_list(Month);
   		false -> MonthStr=integer_to_list(Month)
   end,
   case Day < 10 of
   		true -> DayStr="0"++integer_to_list(Day);
   		false -> DayStr=integer_to_list(Day)
   end,
   DayStr++"."++MonthStr++"."++YearStr.


