[![Build Status](https://travis-ci.org/oltarasenko/erlpop.svg?branch=master)](https://travis-ci.org/oltarasenko/erlpop)

erlpop
============

`erlpop` is a POP3 client library for Erlang. It is derived from the original "epop" Erlang package which includes both a POP server and client.

The original client was heavily refactored, and most of it's code has gone

### Usage ###


```erlang
{ok, Connection} = epop_client2:connect(
    "yourgmail@gmail.com", "password",
    [{addr, "pop.gmail.com"}, {port, 995}, ssl], SocketTimeout).
{ok, EmailsCount} = epop_client2:stat(Connection).
       
{ok, Msg} = epop_client2:retrieve(Connection, <<"1">>).
ok = epop_client2:quit(Connection).
```