-module(util).
-compile(export_all).

system(Format, Data) -> system(io_lib:format(Format, Data)).
system(Command) ->
    io:format("~s~n", [Command]),
    Output = os:cmd(Command),
    io:format(Output),
    Output.

escape([]) -> [];
escape([$\\|Chars]) -> [$\\,$\\|escape(Chars)];
escape([$"|Chars]) -> [$\\,$"|escape(Chars)];
escape([C|Chars]) -> [C|escape(Chars)].

join([String]) -> String;
join([String,[]]) -> String;
join([String|Strings]) ->
    [io_lib:format("~s~n", [String])|join(Strings)].

arglist(Strings) ->
    lists:flatmap(fun(X)->io_lib:format(" \"~s\"", [escape(X)]) end, Strings).

