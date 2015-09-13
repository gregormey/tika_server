-record(verification,{
		mail :: string(),
		timestamp ::non_neg_integer(),
		code :: string()
	}).


-record(day,{
		day :: string(),
		timestamp ::non_neg_integer(),
		checked :: 0 | 1,
		guests = [] :: list()
	}).

-record(user,{
	id :: non_neg_integer(),
	displayName = "" :: string(),
	mail = "" :: string(),
	created :: non_neg_integer(),
	invited :: non_neg_integer(),
	registered :: non_neg_integer(),
	pushToken :: string()
}).


-record(event,{
		id :: non_neg_integer(),
		title :: string(),
		description :: string(),
		dates = [] :: list(),
		answers=0 :: non_neg_integer(),
		contacts=  [] :: list(),
		appointment=0 :: 0 | #day {},
		answer :: 0 | 1,
		creator :: #user {},
		created :: non_neg_integer()
	}).
%% records to store process ids
-record( process_user, {id,pid} ).
-record( process_event, {id,pid} ).
