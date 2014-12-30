-module(tika_user).
-export([invite/2]).
-include("records.hrl").

invite(#user{mail=Mail, displayName=DisplayName},#event{title=Title}) ->
	gen_smtp_client:send({Mail, [], 
					"Subject: "++getSubject(DisplayName,Title)++
					"From: Time Is Knäpp App \r\n"++
					"To: "++DisplayName++" \r\n\r\n"++
					getMailBody()}, 
					[{relay, "localhost"}, {port, 1465}, {ssl, true}]).


getSubject(DisplayName,EventTitle) ->
	DisplayName++" hat dich zu "++EventTitle++" eingeladen.".

getMailBody()->
	"Lade dir die Time Is Knäpp App um zuzusagen.".
	