-module(log).

-export([start/1]).
-include("log.hrl").

start(LogCmd) ->
    io:format(LogCmd),
    process_flag(trap_exit, true),
    Logger = open_port({spawn, lists:flatten(LogCmd)}, [binary, stream]),
    log(Logger, data(Logger)).

data(Logger) ->
    receive
        {Logger, {data, Data}} -> Data;
        {'EXIT', Logger, normal} -> done
    end.

log(Logger, <<           "r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:1/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:2/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:3/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:4/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:5/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:6/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:7/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:8/binary,"r",Data/binary>>) -> log_done(Logger, Data);
log(Logger, <<_:8/binary,    Data/binary>>) -> log(Logger, Data);
log(Logger, <<_:1/binary,    Data/binary>>) -> log(Logger, Data);
log(Logger, <<                          >>) -> log(Logger, data(Logger));
log(_,      done                          ) -> [].

log_done(Logger, Data) -> rev(Logger, #log{}, <<>>, Data).

rev(Logger, Log, A, <<           " | ",Data/binary>>) -> rev_done(Logger, Log, A, <<>>, Data);
rev(Logger, Log, A, <<B:1/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:2/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:3/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:4/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:5/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:6/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:7/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:8/binary," | ",Data/binary>>) -> rev_done(Logger, Log, A, B, Data);
rev(Logger, Log, A, <<B:8/binary,      Data/binary>>) -> rev(Logger, Log, <<A/binary,B/binary>>, Data);
rev(Logger, Log, A, <<B:1/binary,      Data/binary>>) -> rev(Logger, Log, <<A/binary,B/binary>>, Data);
rev(Logger, Log, A, <<                            >>) -> rev(Logger, Log, A, data(Logger)).

rev_done(Logger, Log, A, B, Data) ->
    RevStr = binary_to_list(<<A/binary, B/binary>>),
    {Rev, []} = string:to_integer(RevStr),
    user(Logger, Log#log{rev=Rev}, <<>>, Data).

user(Logger, Log, A, <<           " | ",Data/binary>>) -> date(Logger, Log#log{user=A}, Data);
user(Logger, Log, A, <<B:1/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:2/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:3/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:4/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:5/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:6/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:7/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:8/binary," | ",Data/binary>>) -> user_done(Logger, Log, A, B, Data);
user(Logger, Log, A, <<B:8/binary,      Data/binary>>) -> user(Logger, Log, <<A/binary,B/binary>>, Data);
user(Logger, Log, A, <<B:1/binary,      Data/binary>>) -> user(Logger, Log, <<A/binary,B/binary>>, Data);
user(Logger, Log, A, <<                            >>) -> user(Logger, Log, A, data(Logger)).

user_done(Logger, Log, A, B, Data) ->
    User = <<A/binary,B/binary>>,
    date(Logger, Log#log{user=User}, Data).

date(Logger, Log, <<Date:44/binary," | ",Data/binary>>) -> lines(Logger, Log#log{date=Date}, <<>>, Data);
date(Logger, Log, Data) ->
    MoreData = data(Logger),
    date(Logger, Log, <<Data/binary,MoreData/binary>>).

lines(Logger, Log, <<>>, <<           "1 line",Data/binary>>) -> comment_start(1, Logger, Log#log{lines=1}, Data);
lines(Logger, Log, A,    <<           " lines",Data/binary>>) -> lines_done(Logger, Log, A, <<>>, Data);
lines(Logger, Log, A,    <<B:1/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:2/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:3/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:4/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:5/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:6/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:7/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:8/binary," lines",Data/binary>>) -> lines_done(Logger, Log, A, B,    Data);
lines(Logger, Log, A,    <<B:8/binary,         Data/binary>>) -> lines(Logger, Log, <<A/binary, B/binary>>, Data);
lines(Logger, Log, A,    <<B:1/binary,         Data/binary>>) -> lines(Logger, Log, <<A/binary, B/binary>>, Data);
lines(Logger, Log, A,    <<                               >>) -> lines(Logger, Log, A, data(Logger)).

lines_done(Logger, Log, A, B, Data) ->
    LinesStr = binary_to_list(<<A/binary,B/binary>>),
    {Lines, []} = string:to_integer(LinesStr),
    comment_start(Lines, Logger, Log#log{lines=Lines}, Data).

% handle CR+LF
comment_start(N, Logger, Log, <<"\n\n",    Data/binary>>) -> comment(N, Logger, Log, <<>>, Data);
comment_start(N, Logger, Log, <<"\r\n\r\n",Data/binary>>) -> comment(N, Logger, Log, <<>>, Data);
comment_start(N, Logger, Log, Data) ->
    MoreData = data(Logger),
    comment_start(N, Logger, Log, <<Data/binary,MoreData/binary>>).

% works with CR+LF because CR is implicitly collected
comment(0, Logger, Log, A, Data) -> [Log#log{comment=A}|log(Logger, Data)];
comment(N, Logger, Log, A, <<           "\n",Data/binary>>) -> comment(N-1, Logger, Log, <<A/binary,"\n">>, Data);
comment(N, Logger, Log, A, <<B:1/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:2/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:3/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:4/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:5/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:6/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:7/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:8/binary,"\n",Data/binary>>) -> line(N, Logger, Log, A, B, Data);
comment(N, Logger, Log, A, <<B:8/binary,     Data/binary>>) -> comment(N, Logger, Log, <<A/binary,B/binary>>, Data);
comment(N, Logger, Log, A, <<B:1/binary,     Data/binary>>) -> comment(N, Logger, Log, <<A/binary,B/binary>>, Data);
comment(N, Logger, Log, A, <<                           >>) -> comment(N, Logger, Log, A, data(Logger)).

line(N, Logger, Log, A, B, Data) -> comment(N-1, Logger, Log, <<A/binary,B/binary,"\n">>, Data).
