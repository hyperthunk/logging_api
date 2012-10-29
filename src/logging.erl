%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2000 - 2012 Tim Watson.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------
%% @doc Parse Transform for logging_api - turns the `logging` module into
%% something <em>useful</em> at runtime!
%% ----------------------------------------------------------------------------
-module(logging).

-export([start/0]).
-export([init/0]).
-export([log/3]).

-include_lib("parse_trans/include/codegen.hrl").

log(Level, Fmt, Data) ->
    %% the log/4 function is generated when we call init/0
    apply(?MODULE, log, [Level, [], Fmt, Data]).

start() ->
    ok = application:load(logging_api),
    {ok, App} = application:get_env(logging_api, backend),
    {ok, Keys} = application:get_all_key(logging_api),
    InclKeys = case App of
                   logging -> Keys;
                   _       -> lists:keyreplace(included_applications, 1, Keys,
                                               {included_applications, [App]})
               end,
    application:unload(logging_api),
    application:load({application, logging_api, InclKeys}),
    appstart:start(logging).

init() ->
    case parse_trans_mod:transform_module(?MODULE, [fun transform/2], [return]) of
        ok    -> ignore;
        Error -> {error, {codegen_failed, Error}}
    end.

transform(Forms, _Opts) ->
    Mod = case application:get_env(logging_api, backend) of
              {ok, logging} ->
                  sasl_logging_backend;
              {ok, AppAndModName} ->
                  AppAndModName
          end,

    %% TODO: export/handle add_logger/add_appender and config APIs
    Log4 = {log,
            codegen:gen_function('log',
                                 fun(Level, Meta, Fmt, Data) ->
                                         apply({'$var', Mod}, log,
                                               [Level, Meta, Fmt, Data])
                                 end), 4},

    NewFuns = lists:foldl(
                fun(Level, Acc) ->
                  PrimaryLogFun =
                    codegen:gen_function(
                      Level,
                      fun(Fmt, Data) ->
                              logging:log({'$var', Level}, Fmt, Data)
                      end),
                  ExtraLogFun =
                    codegen:gen_function(
                      Level,
                      fun(Meta, Fmt, Data) ->
                              logging:log({'$var', Level}, Meta, Fmt, Data)
                      end),
                  [{Level, PrimaryLogFun, 2},
                   {Level, ExtraLogFun, 3}|Acc]
                end, [], Mod:levels()) ++ [Log4],

    Forms2 = parse_trans:do_insert_forms(
               below, [F || {_, F, _} <- NewFuns], Forms,
               parse_trans:initial_context(Forms, [])),

    Forms3 = lists:foldl(fun({F, _, Arity}, AccForms) ->
                             parse_trans:export_function(F, Arity, AccForms)
                         end, Forms2, NewFuns),

    parse_trans:revert(Forms3).
