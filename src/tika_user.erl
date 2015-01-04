-module(tika_user).
-export([invite/2]).
-include("records.hrl").

invite(#user{mail=Mail, displayName=DisplayName},#event{title=Title}) ->
	tika_mail:send_invite(Mail,DisplayName,Title).


	