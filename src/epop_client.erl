-module(epop_client).
-author("Oleg Tarasenko <oltarasenko@gmail.com>").


%%%---------------------------------------------------------------------
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%---------------------------------------------------------------------
%% ------------------------------------------
%% Set up a connection to a POP3 server using
%% the specified UserId and Passwd
%% ------------------------------------------

-export([connect/4]).
-export([stat/1]).
-export([retrieve/2]).
-export([quit/1]).
-export([delete/2]).

-record(connection, {
    socket,
    user,            % User name
    addr,            % Address
    passwd,
    port=110,        % The POP3 server port number
    protocol=ssl,     % SSL on/off,
    timeout=10000
}).


-type option()     :: addr | port | ssl.
-type options()    :: {option(), string()} | option().
-type connection() :: #connection{}.

-spec connect(User, Passwd, Options, Timeout) -> Result when
    User    :: string(),
    Passwd  :: string(),
    Options :: options(),
    Timeout :: pos_integer(),
    Result  :: {ok, connection()} | {error, term()}.
connect(User, Passwd, Options, Timeout) ->
    Connection = build_connection_record(User, Passwd, Options, Timeout),

    ConnectionSteps = [
        {get_socket, fun get_socket/1},
        {recv_greetings, fun recv_greetings/1},
        {send_user, fun send_user/1},
        {send_passwd, fun send_passwd/1}
    ],
    case epipe:run(ConnectionSteps, Connection) of
        {error, Step, Reason, _State} ->
            error_logger:error_msg("Failed to establish connection. Reason: ~p", [Step]),
            % Force clean up to make sure that socket is closed
            cleanup(Connection),
            {error, Reason};
        {ok, _Conn} = Success -> Success
    end.


build_connection_record(User, Passwd, Options, Timeout) ->
    Addr = proplists:get_value(addr, Options),
    Port = proplists:get_value(port, Options),
    Protocol = case proplists:get_value(ssl, Options, false) of
        true  -> ssl;
        false -> gen_tcp
    end,
    #connection{
        user = User,
        addr = Addr,
        passwd = Passwd,
        port = Port,
        protocol = Protocol,
        timeout = Timeout
    }.

get_socket(Connection) ->
    #connection{
        addr     = Addr,
        protocol = Protocol,
        timeout  = Timeout,
        port     = Port
    } = Connection,
    ExtraOptions = [binary, {packet, raw}, {reuseaddr, true}, {active, false}],
    case Protocol:connect(Addr, Port, ExtraOptions, Timeout) of
        {ok, Socket} -> {ok, Connection#connection{socket = Socket}};
        Error        -> {error, Error}
    end.

recv_greetings(Connection) ->
    case recv(Connection) of
        {ok, <<"+OK", _Rest/binary>>}   -> {ok, Connection};
        {ok, <<"-ERR ", Error/binary>>} -> {error, Error};
        Err                             -> {error, Err}
    end.

send_user(Connection = #connection{user = User}) ->
    Msg = list_to_binary(User),
    send(Connection, <<"USER ", Msg/binary>>),

    case recv(Connection) of
        {ok, <<"+OK", _Rest/binary>>}   -> {ok, Connection};
        {ok, <<"-ERR ", Error/binary>>} -> {error, Error};
        Err                             -> {error, Err}
    end.

send_passwd(Connection = #connection{passwd = Passwd}) ->
    Msg = list_to_binary(Passwd),
    send(Connection, <<"PASS ", Msg/binary>>),

    case recv(Connection) of
        {ok, <<"+OK", _Rest/binary>>}   -> {ok, Connection};
        {ok, <<"-ERR ", Error/binary>>} -> {error, Error};
        Err                             -> {error, Err}
    end.

% Stat
-spec stat(Connection) -> Result when
    Connection :: connection(),
    Result     :: {ok, NumEmails} | {error, term()},
    NumEmails  :: binary().
stat(Connection) ->
    send(Connection, <<"STAT">>),

    case recv(Connection) of
        {ok, <<"+OK ", Rest/binary>>} ->
            [NumEmails, _] = binary:split(Rest, <<" ">>),
            {ok, NumEmails};

        {ok, <<"-ERR ", Error/binary>>} -> {error, Error};
        Err -> {error, Err}
    end.


% Retrieve
-spec retrieve(Connection, MsgNum) -> Result when
    Connection :: connection(),
    MsgNum     :: binary(),
    Result     :: {ok, binary()} | {error, term()}.
retrieve(Connection, MsgNum) when is_binary(MsgNum) ->
    send(Connection, <<"RETR ", MsgNum/binary>>),

    case recv(Connection) of
        {ok, <<"+OK ", Data/binary>>}   -> maybe_recv_ending(Connection, Data);
        {ok, <<"-ERR ", Error/binary>>} -> {error, Error};
        Error                           -> {error, Error}
    end;
retrieve(_Connection, _MsgNum) -> {error, badarg}.


% Quit
-spec quit(Connection) -> Result when
    Connection :: connection(),
    Result     :: ok | {error, term()}.
quit(Connection) ->
    send(Connection, <<"QUIT">>),

    Resp = case recv(Connection) of
        {ok, <<"-ERR ", Error/binary>>} ->
            {error, Error};
        {ok, <<"+OK">>} ->
            ok;
        Err -> {error, Err}
    end,
    cleanup(Connection),
    Resp.

% Delete
-spec delete(Connection, MsgNum) -> Result when
    Connection :: connection(),
    MsgNum     :: binary(),
    Result     :: ok | {error, term()}.
delete(Connection, MsgNum) when is_binary(MsgNum) ->
    send(Connection, <<"DELE ", MsgNum/binary>>),

    case recv(Connection) of
        {ok, <<"+OK ", _Data/binary>>} -> ok;
        {ok, <<"-ERR ", Err/binary>>} -> {error, Err};
        Error                         -> {error, Error}
    end;
delete(_Connection, _MsgNum) -> {error, badarg}.


% Utils functions

% Helper for closing opened sockets
cleanup(_Connection = #connection{socket = undefined}) ->
    ok;
cleanup(_Connection = #connection{socket = Socket, protocol = Protocol}) ->
    Protocol:close(Socket),
    ok.


% maybe_recv_ending(Connection, Data) when contains_end_octet(Data) == true -> {ok, Data};
maybe_recv_ending(Connection, Data) ->
    case contains_end_octet(Data) of
        true -> {ok, Data};
        false -> recv_ending(Connection, Data)
    end.

recv_ending(Connection = #connection{protocol = Protocol, socket = Socket}, Data) ->
    case Protocol:recv(Socket, 0) of
        {ok, NewData} -> maybe_recv_ending(Connection, <<Data/binary, NewData/binary>>);
        Error         -> {error, Error}
    end.

contains_end_octet(Data) ->
    case binary:match(Data, <<"\r\n.\r\n">>) of
        nomatch -> false;
        _       -> true
    end.

recv(_Meta = #connection{protocol = Protocol, socket = Socket}) ->
    Protocol:recv(Socket, 0).

send(_Meta = #connection{protocol = Protocol, socket = Socket}, Msg) ->
    Protocol:send(Socket, <<Msg/binary, "\r\n">>).
