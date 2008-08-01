-module(util).
-compile(export_all).

system(Format, Data) -> system(io_lib:format(Format, Data)).
system(Command) ->
    io:format("~s~n", [Command]),
    Output = os:cmd(Command),
    io:format(Output),
    Output.

escape(String) ->
    {ok, Escaped, _} = regexp:gsub(String, "\"", "\\\""),
    Escaped.

join(_, [String]) -> String;
join(_, [String,[]]) -> String;
join(Pad, [String|Strings]) ->
    [io_lib:format("~s~s", [String, Pad])|join(Pad, Strings)].

arglist(Strings) ->
    lists:flatmap(fun(X)->io_lib:format(" \"~s\"", [escape(X)]) end, Strings).

