-module(tika_verification).
-export([create_verification/1]).
-export([verify/1]).
-export([list/0]).
-export([load/2]).

-include("records.hrl").


-spec create_verification(string()) -> tuple().
create_verification(Mail) ->
tika_database:write(verification,
					#verification{
						mail=Mail,
						created=tika_database:unixTS(),
						code=get_verification_code(Mail)
					}).

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
	[Verification]=tika_database:find(verification,Fun),
	Verification.

%%internal.
get_verification_code(Mail)->
	hmac:hexlify(hmac:hmac256(integer_to_binary(tika_database:unixTS()),
								list_to_binary(Mail))).

