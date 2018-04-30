all:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit

.PHONY: all clean test
