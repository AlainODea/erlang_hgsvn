-module(svnlog).
-export([repos/1, repos_full/1]).

repos([]) -> {[],[]};
repos([{Rev,Repo}|Repos]) ->
    Logs = logs({Rev,Repo}),
    merge(Repo, Logs, repos(Repos));
repos([Repo|Repos]) ->
    Logs = logs(Repo),
    merge(Repo, Logs, repos(Repos)).

repos_full([]) -> {[],[]};
repos_full([{Rev,Repo}|Repos]) ->
    Logs = all_logs({Rev,Repo}),
    merge(Repo, Logs, repos_full(Repos));
repos_full([Repo|Repos]) ->
    Logs = all_logs(Repo),
    merge(Repo, Logs, repos_full(Repos)).

merge(_, [], {RepoRevs, NextLogs}) -> {RepoRevs, NextLogs};
merge(Repo, [{BaseRev, _, _, _}|_] = Logs, {RepoRevs, NextLogs}) ->
    {[{BaseRev, Repo}|RepoRevs], lists:ukeymerge(1, Logs, NextLogs)}.

logs({End, Repo}) ->
    logs("svn log --stop-on-copy -r BASE:~w \"~s\"", [End, util:escape(Repo)]);
logs(Repo) ->
    logs("svn log --stop-on-copy -r BASE:HEAD \"~s\"", [util:escape(Repo)]).

all_logs(Repo) ->
    logs("svn log --stop-on-copy -r 1:HEAD \"~s\"", [util:escape(Repo)]).

logs(Format, Data) ->
    process_flag(trap_exit, true),
    io:format(Format ++ "~n", Data),
    LogCmd = io_lib:format(Format, Data),
    lists:keysort(1,
        gather_logs(open_port({spawn, lists:flatten(LogCmd)}, [stream]), [])).

gather_logs(Logger, Gathered) ->
    receive
        {Logger, {data, Data}} ->
            {ok, Lines} = regexp:split(Data, "\r\n|\n|\r"),
            gather_logs(Logger, log_info(Lines) ++ Gathered);
        {'EXIT', Logger, normal} -> Gathered
    end.

log_info([]) -> [];
log_info([[]]) -> [];
log_info(["-----" ++ _,[]]) -> [];
log_info(["-----" ++ _,Header,[]|Lines]) ->
    [NumLines,Date,Author,Rev] = metadata(Header),
    {Msg, MoreLogs} = lists:split(NumLines, Lines),
    DateCode = string:substr(Date, 1, 25),
    [{Rev, Author, DateCode, Msg}|log_info(MoreLogs)].

metadata(Header) -> lines([], lists:reverse(Header)).

lines(Lines, " | " ++ Header) ->
    {NumLines, " line" ++ _} = string:to_integer(Lines),
    [NumLines|date([], Header)];
lines(Lines, [Char|Header]) -> lines([Char|Lines], Header).

date(Date, " | " ++ Header) ->
    [Date|user([], Header)];
date(Date, [Char|Header]) -> date([Char|Date], Header).

user(User, [$ ,$|,$ ,Num|Header]) ->
    case string:to_integer([Num]) of
        {error, no_integer} -> user([Num,$ ,$|,$ |User], Header);
        _ -> [User,rev([Num], Header)]
    end;
user(User, [Char|Header]) -> user([Char|User], Header).

rev(Rev, [$r]) ->
    {RevNum, ""} = string:to_integer(Rev),
    RevNum;
rev(Rev, [Char|Header]) -> rev([Char|Rev], Header).

