%%% ==================================
%%% Author: hanyq
%%% Created: 2013-3-29
%%% Description: TODO: Add description to socket_io_listener_sup
%%% ==================================
-module(socket_io_listener_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, {10, Port}).

init({AcceptorCount, Port}) ->

    {ok,
        {
		 {one_for_all, 3, 10},
            [
                {
                    socket_io_acceptor_sup,
                    {socket_io_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [socket_io_acceptor_sup]
                }
			,
                {
                    socket_io_listener,
                    {socket_io_listener, start_link, [AcceptorCount, Port]},
                    transient,
                    100,
                    worker,
                    [socket_io_listener]
                }
            ]
        }
    }.