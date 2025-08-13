-module(myapp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Initialize OpenTelemetry
    opentelemetry:set_default_tracer({opentelemetry, myapp}),
    
    % Start the supervisor
    {ok, Pid} = myapp_sup:start_link(),
    
    % Setup OpenTelemetry Cowboy instrumentation after everything is started
    timer:apply_after(100, opentelemetry_cowboy, setup, []),
    
    {ok, Pid}.

stop(_State) ->
    ok.
