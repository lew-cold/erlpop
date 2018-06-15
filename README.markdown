[![Build Status](https://travis-ci.org/oltarasenko/erlpop.svg?branch=master)](https://travis-ci.org/oltarasenko/erlpop)
[![Hex pm](http://img.shields.io/hexpm/v/erlpop.svg?style=flat)](https://hex.pm/packages/erlpop) [![hex.pm downloads](https://img.shields.io/hexpm/dt/erlpop.svg?style=flat)](https://hex.pm/packages/erlpop)

erlpop
============

`erlpop` is a POP3 client library for Erlang. It is derived from the original "epop" Erlang package which includes both a POP server and client.

The original client was heavily refactored, and most of it's code has gone.


### Usage ###


```erlang
{ok, Connection} = epop_client:connect(
    "yourgmail@gmail.com", "password",
    [{addr, "pop.gmail.com"}, {port, 995}, ssl], SocketTimeout).
{ok, EmailsCount} = epop_client:stat(Connection).
       
{ok, Msg} = epop_client:retrieve(Connection, <<"1">>).
ok = epop_client:quit(Connection).
```
*NOTE*: It's important to call epop_client:quit/1 at the end, as it's responsible for closing (tcp/tls) socket.
