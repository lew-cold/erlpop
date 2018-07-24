-module(client_tests).

-include_lib("eunit/include/eunit.hrl").

 connect_test_() ->
    {foreach,
        fun() ->
            meck:new(gen_tcp, [unstick, passthrough]),
            ok
        end,
        fun(_) -> meck:unload() end,
        [
            {"connection created if all steps return success", fun() ->
                meck:expect(gen_tcp, recv, fun(_, _, _) -> {ok, <<"+OK POP3 ready">>} end),
                Opts = [{addr, "pop.gmail.com"}, {port, 995}],
                ?assertMatch({ok, _}, epop_client:connect("test@example.com", "foo", Opts, 1000))
            end},
            {"-ERR message from socket causes error", fun() ->
                Loop = meck:loop([
                    {ok, <<"+OK POP3 ready">>},
                    {ok, <<"+OK got user">>},
                    {ok, <<"-ERR [AUTH] Username and password not accepted.\r\n">>}
                ]),
                meck:expect(gen_tcp, recv, 3, Loop),
                Opts = [{addr, "pop.gmail.com"}, {port, 995}],
                ?assertMatch(
                    {error, <<"[AUTH] Username and password not accepted.\r\n">>},
                    epop_client:connect("test@example.com", "foo", Opts, 1000)
                )
            end},
            {"Socket errors are handled", fun() ->
                meck:expect(gen_tcp, recv, fun(_, _, _) -> {error, nxdomain} end),
                Opts = [{addr, "pop.gmail.com"}, {port, 995}],
                ?assertMatch(
                    {error, {error, nxdomain}},
                    epop_client:connect("test@example.com", "foo", Opts, 1000)
                )
            end}
        ]
    }.


multiline_commands_test_() ->
    {foreach,
        fun() ->
            meck:new(gen_tcp, [unstick, passthrough]),
            ok
        end,
        fun(_) -> meck:unload() end,
        [
            {"Full multiline message recieved", fun() ->
                meck:expect(gen_tcp, recv, fun(_, _, _) -> {ok, <<"+OK POP3 ready">>} end),
                Opts = [{addr, "pop.gmail.com"}, {port, 995}],
                {ok, Connection} = epop_client:connect("test@example.com", "foo", Opts, 1000),
                Loop = meck:loop([
                    {ok, <<"+OK message follows\r\nMIME-Version: 1.0 \r\n\r\nHi Oleg">>},
                    {ok, <<"\r\n\r\nTips to get the most out of ">>},
                    {ok, <<"Gmail\r\n\r\n -\r\n.\r\n">>}
                ]),
                meck:expect(gen_tcp, recv, 3, Loop),
                ?assertEqual(
                    {ok, <<"message follows\r\nMIME-Version: 1.0 \r\n\r\nHi Oleg\r\n\r\nTips to get the most out of Gmail\r\n\r\n -\r\n.\r\n">>},
                    epop_client:retrieve(Connection, <<"1">>)
                )
            end},
            {"Multiline message socket error in the middle", fun() ->
                meck:expect(gen_tcp, recv, fun(_, _, _) -> {ok, <<"+OK POP3 ready">>} end),
                Opts = [{addr, "pop.gmail.com"}, {port, 995}],
                {ok, Connection} = epop_client:connect("test@example.com", "foo", Opts, 1000),
                Loop = meck:loop([
                    {ok, <<"+OK message follows\r\nMIME-Version: 1.0 \r\n\r\nHi Oleg">>},
                    {ok, <<"\r\n\r\nTips to get the most out of ">>},
                    {error, closed}
                ]),
                meck:expect(gen_tcp, recv, 3, Loop),
                ?assertEqual(
                    {error, {error, closed}},
                    epop_client:retrieve(Connection, <<"1">>)
                )
            end},
            {"Multiline for long messages", fun() -> 
                meck:expect(gen_tcp, recv, fun(_, _, _) -> {ok, <<"+OK POP3 ready">>} end),
                Opts = [{addr, "pop.gmail.com"}, {port, 995}],
                {ok, Connection} = epop_client:connect("test@example.com", "foo", Opts, 1000),
                Msgs = [{ok, <<"+OK Some">>}] ++ [{ok, <<"Some binary">>} || X <- lists:seq(1, 10000)] ++ [{ok, <<"\r\n.\r\n">>}],

                Loop = meck:loop(Msgs),
                meck:expect(gen_tcp, recv, 3, Loop),

                FullMsg = lists:foldl(
                    fun({ok, Msg}, Acc) -> <<Acc/binary, Msg/binary>> end,
                    <<>>, Msgs
                ),
                <<"+OK ", ExpectedMsg/binary>> = FullMsg,
                ?assertEqual(
                    {ok, ExpectedMsg},
                    epop_client:retrieve(Connection, <<"1">>)
                )

            end}
        ]
    }.
