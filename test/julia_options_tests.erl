%%% Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
%%% Copyright (c) 2014, Dreki Þórgísl <dreki@billo.systems>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the copyright holders nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

-module(julia_options_tests).

-include_lib("eunit/include/eunit.hrl").
-include("julia.hrl").
-include("erlport_test_utils.hrl").


parse_test_() ->
    fun () ->
        {ok, #julia_options{julia=Julia, use_stdio=use_stdio,
            call_timeout=infinity, packet=4, julia_path=JuliaPath,
            start_timeout=10000, compressed=0, env=Env,
            port_options=PortOptions,
            buffer_size=65536}} = julia_options:parse([]),
        ?assertPattern(Julia, "/julia(\\.exe)?$"),
        ?assertPattern(JuliaPath, "/priv/julia"),
        ?assertEqual([{"JULIA_LOAD_PATH", JuliaPath}], Env),
        ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
            PortOptions)
    end.

buffer_size_test_() -> [
    ?_assertMatch({ok, #julia_options{buffer_size=65536}},
        julia_options:parse([])),
    ?_assertMatch({ok, #julia_options{buffer_size=5000}},
        julia_options:parse([{buffer_size, 5000}])),
    ?_assertEqual({error, {invalid_option, {buffer_size, 0}}},
        julia_options:parse([{buffer_size, 0}])),
    ?_assertEqual({error, {invalid_option, {buffer_size, invalid}}},
        julia_options:parse([{buffer_size, invalid}]))
    ].

use_stdio_option_test_() -> [
    ?_assertMatch({ok, #julia_options{use_stdio=use_stdio}},
        julia_options:parse([])),
    case os:type() of
        {win32, _} ->
            ?_assertEqual({error, {unsupported_on_this_platform, nouse_stdio}},
                julia_options:parse([nouse_stdio]));
        _ ->
            ?_assertMatch({ok, #julia_options{use_stdio=nouse_stdio}},
                julia_options:parse([nouse_stdio]))
    end,
    ?_assertMatch({ok, #julia_options{use_stdio=use_stdio}},
        julia_options:parse([use_stdio]))
    ].

compressed_option_test_() -> [
    ?_assertMatch({ok, #julia_options{compressed=0}},
        julia_options:parse([])),
    ?_assertMatch({ok, #julia_options{compressed=9}},
        julia_options:parse([{compressed, 9}])),
    ?_assertMatch({error, {invalid_option, {compressed, invalid}}},
        julia_options:parse([{compressed, invalid}]))
    ].

packet_option_test_() -> [
    ?_assertMatch({ok, #julia_options{packet=4}}, julia_options:parse([])),
    ?_assertMatch({ok, #julia_options{packet=4}},
        julia_options:parse([{packet, 4}])),
    ?_assertMatch({ok, #julia_options{packet=1}},
        julia_options:parse([{packet, 1}])),
    ?_assertMatch({ok, #julia_options{packet=2}},
        julia_options:parse([{packet, 2}])),
    ?_assertEqual({error, {invalid_option, {packet, 3}}},
        julia_options:parse([{packet, 3}]))
    ].

start_timeout_test_() -> [
    ?_assertMatch({ok, #julia_options{start_timeout=10000}},
        julia_options:parse([])),
    ?_assertMatch({ok, #julia_options{start_timeout=5000}},
        julia_options:parse([{start_timeout, 5000}])),
    ?_assertMatch({ok, #julia_options{start_timeout=infinity}},
        julia_options:parse([{start_timeout, infinity}])),
    ?_assertEqual({error, {invalid_option, {start_timeout, 0}}},
        julia_options:parse([{start_timeout, 0}])),
    ?_assertEqual({error, {invalid_option, {start_timeout, invalid}}},
        julia_options:parse([{start_timeout, invalid}]))
    ].

call_timeout_test_() -> [
    ?_assertMatch({ok, #julia_options{call_timeout=infinity}},
        julia_options:parse([])),
    ?_assertMatch({ok, #julia_options{call_timeout=5000}},
        julia_options:parse([{call_timeout, 5000}])),
    ?_assertMatch({ok, #julia_options{call_timeout=infinity}},
        julia_options:parse([{call_timeout, infinity}])),
    ?_assertEqual({error, {invalid_option, {call_timeout, 0}}},
        julia_options:parse([{call_timeout, 0}])),
    ?_assertEqual({error, {invalid_option, {call_timeout, invalid}}},
        julia_options:parse([{call_timeout, invalid}]))
    ].

env_option_test_() -> [
    ?_assertMatch({ok, #julia_options{env=[{"JULIA_LOAD_PATH", JuliaPath}],
        julia_path=JuliaPath}}, julia_options:parse([])),
    ?_assertMatch({ok, #julia_options{env=[{"JULIA_LOAD_PATH", JuliaPath},
        {"test", "true"}], julia_path=JuliaPath}},
        julia_options:parse([{env, [{"test", "true"}]}])),
    ?_assertEqual({error, {invalid_option,
        {env, [{"test", "true"}, {test, "true"}, {"test", true}, invalid]},
            [{test, "true"}, {"test", true}, invalid]}},
        julia_options:parse([{env, [{"test", "true"}, {test, "true"},
            {"test", true}, invalid]}])),
    ?_assertEqual({error, {invalid_option, {env, invalid_env}, not_list}},
        julia_options:parse([{env, invalid_env}]))
    ].

julia_option_test_() -> {setup,
    fun () ->
        TmpDir = erlport_test_utils:tmp_dir("erlport_options_tests"),
        BadName = filename:join(TmpDir, "not_executable"),
        ok = file:write_file(BadName, <<>>, [raw]),
        UnknownName = filename:join(TmpDir, "unknown"),
        GoodJulia = erlport_test_utils:create_mock_script(
            "Julia 2.5.0", TmpDir, "julia2"),
        GoodJulia3 = erlport_test_utils:create_mock_script(
            "Julia 3.0.0p1", TmpDir, "julia3"),
        UnsupportedJulia = erlport_test_utils:create_mock_script(
            "Julia 2.4.6", TmpDir, "unsupported"),
        UnsupportedJulia2 = erlport_test_utils:create_mock_script(
            "Julia 4.0.0", TmpDir, "unsupported2"),
        InvalidJulia = erlport_test_utils:create_mock_script(
            "Julia INVALID", TmpDir, "invalid"),
        {TmpDir, GoodJulia, GoodJulia3, BadName, UnknownName,
            UnsupportedJulia, UnsupportedJulia2, InvalidJulia}
    end,
    fun (Info) ->
        ok = erlport_test_utils:remove_object(element(1, Info)) % TmpDir
    end,
    fun ({_, GoodJulia, GoodJulia3, BadName, UnknownName, UnsupportedJulia,
            UnsupportedJulia2, InvalidJulia}) -> [
        fun () ->
            {ok, #julia_options{julia=Julia}} = julia_options:parse([]),
            ?assertPattern(Julia, "/julia(\\.exe)?$")
        end,
        fun () ->
            Expected = erlport_test_utils:script(GoodJulia),
            {ok, #julia_options{julia=Expected, julia_path=JuliaPath}}
                = julia_options:parse([{julia, GoodJulia}]),
            ?assertPattern(JuliaPath, "/priv/julia2")
        end,
        fun () ->
            Expected = erlport_test_utils:script(GoodJulia3),
            {ok, #julia_options{julia=Expected, julia_path=JuliaPath}}
                = julia_options:parse([{julia, GoodJulia3}]),
            ?assertPattern(JuliaPath, "/priv/julia3")
        end,
        fun () ->
            Expected = erlport_test_utils:script(GoodJulia) ++ " -S",
            CommandWithOption = GoodJulia ++ " -S",
            ?assertMatch({ok, #julia_options{julia=Expected}},
                julia_options:parse([{julia, CommandWithOption}]))
        end,
        ?_assertEqual({error, {invalid_option, {julia, BadName}, not_found}},
            julia_options:parse([{julia, BadName}])),
        ?_assertEqual({error, {invalid_option, {julia, UnknownName},
                not_found}},
            julia_options:parse([{julia, UnknownName}])),
        ?_assertEqual({error, {invalid_option,
               {julia, "erlport_tests_unknown_name"}, not_found}},
            julia_options:parse([{julia, "erlport_tests_unknown_name"}])),
        ?_assertEqual({error, {invalid_option, {julia, not_string}}},
            julia_options:parse([{julia, not_string}])),
        fun () ->
            erlport_test_utils:call_with_env(fun () ->
                ?assertEqual({error, julia_not_found},
                    julia_options:parse([]))
                end, "PATH", "")
        end,
        fun () ->
            erlport_test_utils:call_with_env(fun () ->
                ?assertEqual({error, {invalid_env_var,
                    {"ERLPORT_JULIA", "INVALID_julia"}, not_found}},
                    julia_options:parse([]))
                end, "ERLPORT_JULIA", "INVALID_julia")
        end,
        fun () ->
            Expected = erlport_test_utils:script(GoodJulia),
            erlport_test_utils:call_with_env(fun () ->
                ?assertMatch({ok, #julia_options{julia=Expected}},
                    julia_options:parse([]))
                end, "ERLPORT_JULIA", GoodJulia)
        end,
        ?_assertEqual({error, {unsupported_julia_version, "Julia 0.2.9"}},
            julia_options:parse([{julia, UnsupportedJulia}])),
        ?_assertEqual({error, {invalid_julia,
                erlport_test_utils:script(InvalidJulia)}},
            julia_options:parse([{julia, InvalidJulia}]))
    ] end}.

cd_option_test_() -> {setup,
    fun () ->
        erlport_test_utils:tmp_dir("erlport_options_tests")
    end,
    fun erlport_test_utils:remove_object/1,
    fun (TmpDir) -> [
        fun () ->
            {ok, #julia_options{cd=undefined, port_options=PortOptions,
                env=Env}} = julia_options:parse([]),
            ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
                PortOptions)
        end,
        fun () ->
            {ok, #julia_options{cd=TmpDir, port_options=PortOptions, env=Env}}
                = julia_options:parse([{cd, TmpDir}]),
            ?assertEqual([{env, Env}, {packet, 4}, {cd, TmpDir},
                binary, hide, exit_status], PortOptions)
        end,
        ?_assertEqual({error, {invalid_option, {cd, "invalid_directory"}}},
            julia_options:parse([{cd, "invalid_directory"}]))
    ] end}.

julia_path_option_test_() -> {setup,
    fun () ->
        TmpDir = erlport_test_utils:tmp_dir("erlport_options_tests"),
        TestPath1 = filename:join(TmpDir, "path1"),
        ok = file:make_dir(TestPath1),
        TestPath2 = filename:join(TmpDir, "path2"),
        ok = file:make_dir(TestPath2),
        {TmpDir, TestPath1, TestPath2}
    end,
    fun (Info) ->
        ok = erlport_test_utils:remove_object(element(1, Info)) % TmpDir
    end,
    fun ({_, TestPath1, TestPath2}) -> [
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse([]),
            ?assertPattern(JuliaPath, "/priv/julia")
        end,
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse(
                    [{julia_path, [TestPath1]}]),
            ?assertPattern(JuliaPath, ["/priv/julia", TestPath1])
        end,
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse(
                    [{julia_path, TestPath1}]),
            ?assertPattern(JuliaPath, ["/priv/julia", TestPath1])
        end,
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse(
                    [{julia_path, erlport_test_utils:local_path(
                        [TestPath1, TestPath2])}]),
            ?assertPattern(JuliaPath,
                ["/priv/julia", TestPath1, TestPath2])
        end,
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse(
                    [{julia_path, [TestPath1]},
                    {env, [{"JULIA_LOAD_PATH", TestPath2}]}]),
            ?assertPattern(JuliaPath,
                ["/priv/julia", TestPath1, TestPath2])
        end,
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse(
                    [{env, [{"JULIA_LOAD_PATH", TestPath1},
                    {"JULIA_LOAD_PATH", TestPath2}]}]),
            ?assertPattern(JuliaPath,
                ["/priv/julia", TestPath1, TestPath2])
        end,
        fun () ->
            {ok, #julia_options{julia_path=JuliaPath,
                env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                port_options=[{env, Env} | _]}} = julia_options:parse(
                    [{julia_path, [TestPath1, TestPath2, ""]},
                    {env, [{"JULIA_LOAD_PATH", erlport_test_utils:local_path(
                        [TestPath2, TestPath1])}]}]),
            ?assertPattern(JuliaPath,
                ["/priv/julia", TestPath1, TestPath2])
        end,
        fun () ->
            erlport_test_utils:call_with_env(fun () ->
                {ok, #julia_options{julia_path=JuliaPath,
                    env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                    port_options=[{env, Env} | _]}} = julia_options:parse([]),
                ?assertPattern(JuliaPath, "/priv/julia")
                end, "JULIA_LOAD_PATH", "")
        end,
        fun () ->
            erlport_test_utils:call_with_env(fun () ->
                {ok, #julia_options{julia_path=JuliaPath,
                    env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                    port_options=[{env, Env} | _]}} = julia_options:parse([]),
                ?assertPattern(JuliaPath, ["/priv/julia", TestPath1])
                end, "JULIA_LOAD_PATH", TestPath1)
        end,
        fun () ->
            erlport_test_utils:call_with_env(fun () ->
                {ok, #julia_options{julia_path=JuliaPath,
                    env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                    port_options=[{env, Env} | _]}} = julia_options:parse([]),
                ?assertPattern(JuliaPath,
                    ["/priv/julia", TestPath1, TestPath2])
                end, "JULIA_LOAD_PATH", erlport_test_utils:local_path(
                    [TestPath1, TestPath2]))
        end,
        fun () ->
            erlport_test_utils:call_with_env(fun () ->
                {ok, #julia_options{julia_path=JuliaPath,
                    env=[{"JULIA_LOAD_PATH", JuliaPath}]=Env,
                    port_options=[{env, Env} | _]}} = julia_options:parse(
                        [{julia_path, TestPath1}]),
                ?assertPattern(JuliaPath,
                    ["/priv/julia", TestPath1, TestPath2])
                end, "JULIA_LOAD_PATH", TestPath2)
        end,
        ?_assertEqual({error, {invalid_option, {julia_path, invalid_path},
                not_list}},
            julia_options:parse([{julia_path, invalid_path}])),
        ?_assertEqual({error, {invalid_option, {julia_path, ""},
                invalid_path}},
            julia_options:parse([{julia_path, ""}])),
        ?_assertEqual({error, {invalid_option, {julia_path,
                [TestPath1, invalid]}, [invalid]}},
            julia_options:parse([{julia_path, [TestPath1, invalid]}])),
        ?_assertEqual({error, {invalid_option, {julia_path,
                [$a, $b, invalid]}, [invalid]}},
            julia_options:parse([{julia_path, [$a, $b, invalid]}])),
        fun () ->
            Dir = code:lib_dir(erlport),
            ok = erlport_test_utils:del_code_path(erlport, 5),
            try ?assertEqual({error, {not_found, "erlport/priv"}},
                    julia_options:parse([]))
            after
                true = code:add_patha(Dir)
            end
        end
    ] end}.

unknown_option_test_() ->
    ?_assertEqual({error, {unknown_option, unknown}},
        julia_options:parse([unknown])).
