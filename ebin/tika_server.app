{application,tika_server,
             [{description,"Backend for Time is Knäpp App"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{tika_server_app,[]}},
              {env,[]},
              {modules,[tika_event,tika_event_fsm,tika_server_app,
                        tika_server_sup,tika_user,tika_user_fsm]}]}.