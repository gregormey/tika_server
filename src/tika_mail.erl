-module(tika_mail).
-export([send_invite/4]).
-export([send_verification/3]).

send_invite(Mail,CreatorName,UserName,EventTitle) ->
	send(Mail,getInviteMailContent(Mail, CreatorName,UserName,EventTitle)).
send_verification(Mail,UserName,Link) ->
	send(Mail,getVerifyMailContent(Mail,UserName,Link)).

send(Mail,Content)->
	Email = {
    	Mail,
    	[Mail],
    	Content
	},
	Options = [
	   	{ssl,true},
    	{no_mx_lookups,true},
    	{relay,"smtp.gmail.com"},
    	{username,"timeisknaepp@gmail.com"},
    	{password,"EGv71DoSFaUIPa"},
    	{auth,always}
	],
	%%gen_smtp_client:send_blocking(Email, Options).
	gen_smtp_client:send(Email, Options).

getInviteMailContent(Mail, CreatorName,UserName,EventTitle)->
	From="From: TIKA <timeisknaepp@gmail.com>\r\n",
	To="To: "++UserName++" <"++Mail++">\r\n",
	Subject="Subject: "++CreatorName++" hat dich zu "++EventTitle++" eingeladen.\r\n\r\n",
	Text="Lade dir die Time Is Knäpp App im App Store oder im Google Play Store um zuzusagen.",
	Imprint="LG\r\nTika",
	list_to_binary(From++To++Subject++Text++Imprint). 

getVerifyMailContent(Mail,UserName,Link)->
	From="From: TIKA <timeisknaepp@gmail.com>\r\n",
	To="To: "++UserName++" <"++Mail++">\r\n",
	Subject="Subject: Verifiziere deine Adresse.\r\n\r\n",
	Text="Klicke hier "++Link++" um deine E-Mail-Adresse zu verifizieren.",
	Imprint="LG\r\nTika",
	list_to_binary(From++To++Subject++Text++Imprint). 

