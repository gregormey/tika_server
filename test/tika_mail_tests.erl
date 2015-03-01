-module(tika_mail_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

send_invite_test()->
	ok=application:start(asn1),
	ok=application:start(public_key),
	ok=application:start(ssl),
	Response=tika_mail:send_invite("gregor@meyenberg.de", "Gregor","Ein Event"),
	?assert(string:str(binary_to_list(Response), "OK") > 0).