-module(sdd_json).

-export([decode/1, encode/1]).

%% ------------------------------------------------------------------------------------- %%
%% decode

%% Decode JSON to Term

decode(JSON) ->
	jsx:decode(JSON).

%% ------------------------------------------------------------------------------------- %%
%% encode

%% encode Term to JSON
%% but first pre_encode it

encode(Term) ->
	Term1 = pre_encode(Term),
	jsx:encode(Term1).

%% ------------------------------------------------------------------------------------- %%
%% pre_encode

%% Convert Terms to a valid Erlang representation of JSON

pre_encode(Part) when is_tuple(Part) ->
	List = case array:is_array(Part) of
		true ->
			array:to_list(Part);
		false ->
			tuple_to_list(Part)
	end,
	lists:map(fun pre_encode/1, List);

pre_encode(Part) when is_list(Part) ->
	lists:map(fun pre_encode/1, Part);

pre_encode(Part) when is_atom(Part) ->
	atom_to_binary(Part, latin1);

pre_encode(Part) when is_number(Part) ->
	Part;

pre_encode(Part) when is_binary(Part) ->
	Part;

pre_encode(Part) ->
	lists:flatten(io_lib:format("~p", [Part])).