-module(tika_config).
-author("Gregor Meyenberg <gregor@meyenberg.de>").

-export([get_value/3]).



%% read priv/yags.config file
-spec config_file(tika, File::string()) -> tuple()| {error, any()}.
config_file(App,File) ->
    case file:consult(filename:join([priv_dir(App), File])) of
        {ok, Terms} ->
            Terms;
        Else ->
            Else
    end.

%% find the path to the priv directory in an application
-spec priv_dir(yags) -> string().
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(App)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.

%% loads a value from yags.config
-spec get_value(config|list(),list()|tuple(),any())-> any().
get_value(config,Keys,Default) ->
	Config=config_file(tika,"tika.config"),
	get_value(Keys,Config,Default);


 %% recrusive search for value in a config set 
 get_value([Key|Keys],Opts,Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, V} -> get_value(Keys,V,Default);
        _ -> get_value([],Default,Default)
    end;

 get_value([],V,_) -> V.