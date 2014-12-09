-record(day,{
		day :: string(),
		timestamp ::non_neg_integer(),
		checked :: 0 | 1,
		guests = [] :: list()
	}).

-record(user,{
	id :: non_neg_integer(),
	displayName :: string(),
	mail :: string()
}).

-record(answers,{
		positive :: non_neg_integer(),
		negative :: non_neg_integer()
	}).

-record(event,{
		id :: non_neg_integer(),
		title :: string(),
		description :: string(),
		dates = [] :: list(),
		answers :: #answers {},
		contacts:[] :: list(),
		appointment :: 0 | 1,
		answer :: 0 | 1,
		creator :: #user {}
	}).