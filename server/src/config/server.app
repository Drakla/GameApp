{
    application, server,
    [
        {description, "This is game server."},   
        {vsn, "1.0a"},   
        {modules,
            [server ]
            },   
        {registered, [server_sup]},   
        {applications, [kernel, stdlib, sasl]},   
        {mod, {server_app, []}},   
        {start_phases, []}
    ]
}.

%% File end.
