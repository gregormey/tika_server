-module(tika_user).
-export([invite/2,create/0]).
-include("records.hrl").

-type user() :: #user {}.

invite(#user{mail=Mail, displayName=DisplayName},#event{title=Title}) ->
	tika_mail:send_invite(Mail,DisplayName,Title).

-spec create() ->  user().
create()->
	tika_database:create(user,#user{}).
	