-module(myapp_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, myapp_sup}, myapp_sup, []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", handler, []},
            {"/param/:id", handler, [{action, param}]},
            {"/exception", handler, [{action, exception}]}
        ]}
    ]),
    
    % Configure Cowboy with standard options (OpenTelemetry auto-instrumentation via setup/0)
    CowboyOpts = #{
        env => #{dispatch => Dispatch}
    },
    
    CowboySpec = #{
        id => cowboy_http_listener,
        start => {cowboy, start_clear, [http_listener, [{port, 8080}], CowboyOpts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cowboy]
    },
    {ok, {{one_for_one, 5, 10}, [CowboySpec]}}.
