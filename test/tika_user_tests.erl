-module(tika_user_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

user()->
	#user{
		displayName = "Gregor Meyenberg",
		mail="gregor@meyenberg.de"
	}.

event()->
	#event{
		title="Test Event"
	}.

mail_test_()->
	{setup,
		fun start/0,
		fun stop/1,
		[
			fun () ->
				getSubject(),
				getMailBody(),
				invite()
			end
		]
	}.

start() ->
	%gen_smtp_server:start(smtp_server_tika, 
	%						[[{sessionoptions, 
	%								[{allow_bare_newlines, fix}, 
	%									{callbackoptions, 
	%									[{parse, true}]}
	%								]
	%							}]
	%						]).
	{ok,Pid}=gen_smtp_server:start(smtp_server_tika, [[], [{protocol, ssl}, {port, 1465}]]),
	Pid.
 
stop(Pid) ->
	gen_smtp_server:stop(Pid).

getSubject()->
	User=user(),
	Event=event(),
	Subject=tika_user:getSubject(User#user.displayName,Event#event.title),
	?_assertEqual(Subject,"Gregor Meyenberg hat dich zu Test Event eingeladen.").

getMailBody()->
	MailBody=tika_user:getMailBody(),
	?_assertEqual(MailBody,"Lade dir die Time Is Knäpp App um zuzusagen.").

invite() ->
	?_assert( ok == tika_user:invite(user(),event())).




