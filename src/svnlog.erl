-module(svnlog).
-export([repos/1, repos_full/1]).
-include("log.hrl").

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
merge(Repo, [#log{rev=BaseRev}|_] = Logs, {RepoRevs, NextLogs}) ->
    {[{BaseRev, Repo}|RepoRevs], lists:ukeymerge(2, Logs, NextLogs)}.

logs({End, Repo}) ->
    logs("svn log --stop-on-copy -r BASE:~w \"~s\"", [End, util:escape(Repo)]);
logs(Repo) ->
    logs("svn log --stop-on-copy -r BASE:HEAD \"~s\"", [util:escape(Repo)]).

all_logs(Repo) ->
    logs("svn log --stop-on-copy -r 1:HEAD \"~s\"", [util:escape(Repo)]).

logs(Format, Data) ->
    process_flag(trap_exit, true),
    io:format(Format ++ "~n", Data),
    lists:keysort(2, log:start(io_lib:format(Format, Data))).
