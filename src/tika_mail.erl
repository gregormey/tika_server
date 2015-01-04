-module(tika_mail).
-export([send_invite/3]).

send_invite(Mail, UserName,EventTitle) ->
	Email = {
    	Mail,
    	[Mail],
    	getMailContent(Mail, UserName,EventTitle)
	},
	Options = [
	   	{ssl,true},
    	{no_mx_lookups,true},
    	{relay,"smtp.gmail.com"},
    	{username,"timeisknaepp@gmail.com"},
    	{password,"EGv71DoSFaUIPa"},
    	{auth,always}
	],
	gen_smtp_client:send_blocking(Email, Options).

getMailContent(Mail, UserName,EventTitle)->
	From="From: TIKA <timeisknaepp@gmail.com>\r\n",
	To="To: "++UserName++" <"++Mail++">\r\n",
	Subject="Subject: "++UserName++" hat dich zu "++EventTitle++" eingeladen.\r\n\r\n",
	Text="Lade dir die Time Is Knäpp App im App Store oder im Google Play Store um zuzusagen.",
	Imprint="LG\r\nTika",
	list_to_binary(From++To++Subject++Text++Imprint). 

