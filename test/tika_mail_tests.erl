-module(tika_mail_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

send_invite_test()->
	tika:start_server(),
	Response=tika_mail:send_invite("gregor@meyenberg.de", "Gregor", "Gregor", "Ein Event"),
	?assert(string:str(binary_to_list(Response), "OK") > 0),
	tika:stop_server().