-module(tika_verification).
-export([create_verification/1]).
-export([verify/1]).
-export([list/0]).
-export([load/2]).
-export([remove/2]).

-include("records.hrl").
-define(VERIFICATION_BASE_URL, "http://localhost:8080/#/verify/").

-spec create_verification(string()) -> tuple().
create_verification(Mail) ->
[Verification]=tika_database:write(verification,
					#verification{
						mail=Mail,
						created=tika_database:unixTS(),
						code=get_verification_code(Mail)
					}),
User=tika_user:load(mail,Verification#verification.mail), 
tika_mail:send_verification(User#user.mail,
					User#user.displayName,
					?VERIFICATION_BASE_URL++Verification#verification.code).

-spec verify(string()) -> tuple() | not_found.
verify(Code)->
Fun=fun(R) ->
			Code==R#verification.code
	end,
case tika_database:find(verification,Fun) of
	not_found -> not_found;
	[Verification] -> tika_database:write(verification,
					Verification#verification{
						verified=tika_database:unixTS()
					}),
					ok
end.

list()->
	tika_database:find(verification).

load(mail,Mail)->
	Fun=fun(R) ->
				Mail==R#verification.mail
		end,
	case tika_database:find(verification,Fun) of
		[Verification] -> Verification;
		not_found -> not_found
	end.
remove(mail,Mail)->
	tika_database:delete(verification,#verification{mail=Mail}).


%%internal.
get_verification_code(Mail)->
	hmac:hexlify(hmac:hmac256(integer_to_binary(tika_database:unixTS()),
								list_to_binary(Mail))).

