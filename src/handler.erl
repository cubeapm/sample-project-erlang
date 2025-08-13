-module(handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    
    % Test tracer availability
    Tracer = opentelemetry:get_application_tracer(?MODULE),
    logger:info("Tracer for ~p: ~p", [?MODULE, Tracer]),
    
    % Log the incoming request
    logger:info("~s ~s", [Method, Path]),
    
    % Handle different endpoints based on the action in State
    case proplists:get_value(action, State) of
        param ->
            handle_param(Req, State);
        exception ->
            handle_exception(Req, State);
        _ ->
            handle_default(Req, State)
    end.

handle_default(Req, State) ->
    ?with_span(<<"GET /">>, #{kind => server}, fun(_SpanCtx) ->
        ?set_attributes([{<<"http.method">>, <<"GET">>},
                         {<<"http.route">>, <<"/">>},
                         {<<"operation">>, <<"handle_default">>}]),
        ?add_event(<<"Processing default endpoint">>, []),
        
        Req2 = cowboy_req:reply(200,
                                #{<<"content-type">> => <<"text/plain">>},
                                <<"Hello from OTP24 Cowboy!">>,
                                Req),
        {ok, Req2, State}
    end).

handle_param(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    logger:info("Parameter received: ~s", [Id]),
    
    ?with_span(<<"GET /param/:id">>, #{kind => server}, fun(_SpanCtx) ->
        ?set_attributes([{<<"http.method">>, <<"GET">>},
                         {<<"http.route">>, <<"/param/:id">>},
                         {<<"param.id">>, Id},
                         {<<"operation">>, <<"handle_param">>}]),
        ?add_event(<<"Processing parameter endpoint">>, [{<<"param_value">>, Id}]),
        
        % Simple telemetry event for parameter handling
        telemetry:execute([myapp, parameter, received], #{count => 1}, #{param_id => Id}),
        
        Response = iolist_to_binary(["Parameter logged: ", Id]),
        Req2 = cowboy_req:reply(200,
                                #{<<"content-type">> => <<"text/plain">>},
                                Response,
                                Req),
        {ok, Req2, State}
    end).

handle_exception(_Req, _State) ->
    logger:error("Exception endpoint called - throwing error!"),
    
    ?with_span(<<"GET /exception">>, #{kind => server}, fun(_SpanCtx) ->
        ?set_attributes([{<<"http.method">>, <<"GET">>},
                         {<<"http.route">>, <<"/exception">>},
                         {<<"operation">>, <<"handle_exception">>},
                         {<<"error.type">>, <<"intentional_exception">>}]),
        ?add_event(<<"About to throw intentional exception">>, []),
        ?set_status(?OTEL_STATUS_ERROR, <<"Intentional exception for testing">>),
        
        % Simple telemetry event for exception
        telemetry:execute([myapp, exception, thrown], #{count => 1}, #{error_type => intentional_exception}),
        
        error(intentional_exception)
    end).
