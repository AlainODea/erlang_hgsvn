-module(hgsvn).
-export([start/0]).

start() -> run(init:get_argument(watch),
               repo_sets(init:get_argument(repo_set)),
               stop_sets(init:get_argument(stop_set))).

run({ok,[[Watch]]}, RepoSets, [[]]) ->
    Sleep = time(string:to_integer(Watch)),
    watch(Sleep, RepoSets);
run({ok,[[_]]}, _, _) ->
    exit({watch,notSupported,with,stop_set});
run(error, RepoSets, StopSets) ->
    commit_sets(RepoSets, StopSets).

watch(Sleep, RepoSets) ->
    commit_sets(RepoSets, [[]]),
    receive
        after Sleep ->
           watch(Sleep, RepoSets)
    end.

repo_sets(error) -> [["."]];
repo_sets({ok, RepoSets}) -> RepoSets.

stop_sets(error) -> [[]];
stop_sets({ok, StopRevs}) ->
    lists:map(fun(StopSet) ->
        lists:map(fun(StopRevStr) ->
            {StopRev,[]} = string:to_integer(StopRevStr),
            StopRev
        end, StopSet)
    end, StopRevs).

time({Amount, []}) -> Amount;
time({Amount, "s"}) -> Amount * 1000;
time({Amount, "min"}) -> Amount * 1000 * 60;
time({Amount, "hr"}) -> Amount * 1000 * 60 * 60;
time(Time) -> exit({badTime, Time}).

commit_sets([], []) -> ok;
commit_sets([Repos|RepoSets], []) ->
    commit_set(Repos, []),
    commit_sets(RepoSets, []);
commit_sets([Repos|RepoSets], [StopRevs|StopSets]) ->
    commit_set(Repos, StopRevs),
    commit_sets(RepoSets, StopSets).

commit_set(Repos, []) -> commit_set(Repos);
commit_set(Repos, StopRevs) ->
    commit_set(lists:keysort(1, lists:zip(StopRevs, Repos))).

commit_set(Repos) ->
    {BaseRevs, Logs} = svnlog:repos(Repos),
    commit(Logs, lists:keysort(1, BaseRevs), [], []).

commit([], _, _, _) -> ok;
commit(_, [], _, []) -> ok;
commit([{Rev, _, _, _}|_] = Logs, BaseRevs, [{Rev, Repo}|StopRevs], Repos) ->
    commit(Logs, BaseRevs, StopRevs,
        lists:filter(fun(ARepo) -> ARepo =/= Repo end, Repos));
commit([{Rev, _, _, _}|_] = Logs, [{Rev, Repo}|BaseRevs], StopRevs, Repos) ->
    commit(Logs, BaseRevs, StopRevs, [Repo|Repos]);
commit([{Rev, Author, Date, Msg}|Logs], BaseRevs, StopRevs, Repos) ->
    util:system("svn up -r ~w~s", [Rev, util:arglist(Repos)]),
    util:system("hg addremove"),
    util:system("hg ci -u \"~s\" -d \"~s\" -m \"~s\"",
        lists:map(fun util:escape/1, [Author, Date,
            [io_lib:format("[SVN r~w]", [Rev])|util:join(util:escape(Msg))]])),
    commit(Logs, BaseRevs, StopRevs, Repos).

