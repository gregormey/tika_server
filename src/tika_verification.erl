-module(tika_verification).
-export([create_verification/1]).
-export([verify/1]).
-export([list/0]).

-include("records.hrl").


-spec create_verification(string()) -> tuple.
create_verification(Mail) ->
tika_database:write(verification,
					#verification{
						mail=Mail,
						timestamp=tika_database:unixTS(),
						code=get_verification_code(Mail)
					}).

-spec verify(string()) -> ok | not_found.
verify(Code)->
Fun=fun(R) ->
			Code==R#verification.code
	end,
case tika_database:find(verification,Fun) of
	not_found -> not_found;
	[Verification] -> 
					tika_database:delete(verification, Verification)
end.

list()->
	tika_database:find(verification).

%%internal.
get_verification_code(Mail)->
	hmac:hexlify(hmac:hmac256(integer_to_binary(tika_database:unixTS()),
								list_to_binary(Mail))).

