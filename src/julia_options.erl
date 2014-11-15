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

%%%
%%% @doc Julia options handling
%%% @author Dreki Þórgísl <dreki@billo.systems>
%%% @copyright 2009-2013 Dmitry Vasiliev <dima@hlabs.org>,
%%%            2014 Dreki Þórgísl <dreki@billo.systems>
%%% @private
%%%

-module(julia_options).

-author('Dreki Þórgísl <dreki@billo.systems>').

-export([
    parse/1
    ]).

-define(TIMEOUT, 15000).

-type option() :: {julia, Julia :: string()}
    | {julia_path, Path :: string() | [Path :: string()]}
    | erlport_options:option().
-type options() :: [option()].

-export_type([option/0, options/0]).

-include("julia.hrl").


%%
%% @doc Parse Julia options
%%

-spec parse(Options::options()) ->
    {ok, #julia_options{}} | {error, Reason::term()}.

parse(Options) when is_list(Options) ->
    parse(Options, #julia_options{}).

parse([{julia, Julia} | Tail], Options) ->
    % Will be checked later
    parse(Tail, Options#julia_options{julia=Julia});
parse([{julia_path, JuliaPath}=Value | Tail], Options) ->
    case erlport_options:filter_invalid_paths(JuliaPath) of
        {ok, Path} ->
            % Paths will be checked later
            parse(Tail, Options#julia_options{julia_path=Path});
        {error, Invalid} ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse([Option | Tail], Options) ->
    case erlport_options:parse(Option) of
        {ok, Name, Value} ->
            parse(Tail, set_by_name(Name, Value, Options));
        {error, _}=Error ->
            Error
    end;
parse([], Options=#julia_options{env=Env0, julia_path=JuliaPath0,
        julia=Julia, port_options=PortOptions, packet=Packet,
        cd=Path, use_stdio=UseStdio}) ->
    PortOptions1 = erlport_options:update_port_options(
        PortOptions, Path, UseStdio),
    case get_julia(Julia) of
        {ok, JuliaFilename, MajVersion} ->
            case update_julia_path(Env0, JuliaPath0, MajVersion) of
                {ok, JuliaPath, Env} ->
                    {ok, Options#julia_options{env=Env,
                        julia_path=JuliaPath, julia=JuliaFilename,
                        port_options=[{env, Env}, {packet, Packet}
                            | PortOptions1]}};
                {error, _}=Error ->
                    Error
            end;
        {error, _}=Error ->
            Error
    end.

%%%
%%% Utility functions
%%%

set_by_name(Name, Value, Options) ->
    case proplists:get_value(Name, ?JULIA_FIELDS) of
        N when is_integer(N) andalso N > 1 ->
            setelement(N, Options, Value)
    end.

update_julia_path(Env0, JuliaPath0, MajVersion) ->
    case code:priv_dir(erlport) of
        {error, bad_name} ->
            {error, {not_found, "erlport/priv"}};
        PrivDir ->
            JuliaDir = lists:concat([julia, MajVersion]),
            ErlPortPath = erlport_options:joinpath(PrivDir, JuliaDir),
            {PathFromSetEnv, Env2} = extract_julia_path(Env0, "", []),
            PathFromEnv = erlport_options:getenv("JULIA_LOAD_PATH"),
            JuliaPath = erlport_options:join_path([[ErlPortPath], JuliaPath0,
                erlport_options:split_path(PathFromSetEnv),
                erlport_options:split_path(PathFromEnv)]),
            Env3 = [{"JULIA_LOAD_PATH", JuliaPath} | Env2],
            {ok, JuliaPath, Env3}
    end.

get_julia(default) ->
    case erlport_options:getenv(?JULIA_VAR_NAME) of
        "" ->
            try find_julia(?DEFAULT_JULIA)
            catch
                throw:not_found ->
                    {error, julia_not_found}
            end;
        Julia ->
            try find_julia(Julia)
            catch
                throw:not_found ->
                    {error, {invalid_env_var, {?JULIA_VAR_NAME, Julia},
                        not_found}}
            end
    end;
get_julia(Julia=[_|_]) ->
    try find_julia(Julia)
    catch
        throw:not_found ->
            {error, {invalid_option, {julia, Julia}, not_found}}
    end;
get_julia(Julia) ->
    {error, {invalid_option, {julia, Julia}}}.

find_julia(Julia) ->
    {JuliaCommand, Options} = lists:splitwith(fun (C) -> C =/= $ end, Julia),
    case os:find_executable(JuliaCommand) of
        false ->
            throw(not_found);
        Filename ->
            Fullname = erlport_options:absname(Filename),
            case check_julia_version(Fullname) of
                {ok, {MajVersion, _, _}} ->
                    {ok, Fullname ++ Options, MajVersion};
                {error, _}=Error ->
                    Error
            end
    end.

extract_julia_path([{"JULIA_LOAD_PATH", P} | Tail], Path, Env) ->
    extract_julia_path(Tail, [P, erlport_options:pathsep() | Path], Env);
extract_julia_path([Item | Tail], Path, Env) ->
    extract_julia_path(Tail, Path, [Item | Env]);
extract_julia_path([], Path, Env) ->
    {lists:append(lists:reverse(Path)), lists:reverse(Env)}.

check_julia_version(Julia) ->
    Out = erlport_options:get_version(Julia ++ " -v"),
    case re:run(Out, "^julia version ([0-9]+)\\.([0-9]+)\\.([0-9]+)",
            [{capture, all_but_first, list}]) of
        {match, StrVersion} ->
            Version = list_to_tuple([list_to_integer(N) || N <- StrVersion]),
            if
                Version >= {0, 3, 2} ->
                    {ok, Version};
                true ->
                    {error, {unsupported_julia_version, Out}}
            end;
        nomatch ->
            {error, {invalid_julia, Julia}}
    end.
