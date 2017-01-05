defmodule Tellet.Service do

  use Tellet
  use Tellet.Code
  import Logger

  @gen_server_opts timeout: 10_000

  defmacro __using__(opts) do
    quote do
      import unquote(__MODULE__)
    end
  end # defmacro

  defmacro defun(head, body) do
    {fun, args_ast} = name_and_args(head)
    [args, state, conds] = case args_ast do
      [args, state] -> [args, state, []]; _ -> args_ast
    end

    quote do
      def unquote(fun)(var!(unquote args), var!(unquote state)) do
        #{cmd_, state_} = (unquote body[:do])
        #{cmd_, unquote(fun)({:after!, var!(unquote args), state_}) || state_}
        unquote body[:do]
      end

      def unquote(fun)({:check!, var!(unquote args)}) do
        Tellet.Validator.check!(unquote(fun), var!(unquote args), unquote(conds))
      end

      def unquote(fun)(_), do: nil
    end
  end

  defmacro defunp(head, body) do
    {fun, args_ast} = name_and_args(head)
    [args, state, conds] = case args_ast do
      [args, state] -> [args, state, []]; _ -> args_ast
    end

    quote do
      def unquote(fun)(var!(unquote args), var!(unquote state)) do
        Tellet.Service.check_auth! var!(state)
        #{cmd_, state_} = (unquote body[:do])
        #{cmd_, unquote(fun)({:after!, var!(unquote args), state_}) || state_}
        unquote body[:do]
      end

      def unquote(fun)({:check!, var!(unquote args)}) do
        Tellet.Validator.check!(unquote(fun), var!(unquote args), unquote(conds))
      end

      def unquote(fun)(_), do: nil
    end
  end

  defmacro predef(head, body) do
    {fun, args_ast} = name_and_args(head)
    [args, state, conds] = case args_ast do
      [args, state] -> [args, state, []]; _ -> args_ast
    end
    quote do
      def unquote(fun)({:predef!, var!(unquote args), var!(unquote state)}) do
        unquote(body[:do])
      end
    end
  end

  defmacro max_nodes, do: 10_000
  defmacro max_hops, do: 100_000

  defmacro enter!(state), do: quote do: {:enter, unquote state}
  defmacro next!(state), do: quote do: {:next, unquote state}
  defmacro repeat!(state), do: quote do: {:repeat, unquote state}
  defmacro escape!(state), do: quote do: {:escape, unquote state}
  defmacro break!(state), do: quote do: {:break, unquote state}
  defmacro stop!(state), do: quote do: {:stop, unquote state}
  defmacro error!(state), do: quote do: {:error, unquote state}

  defp name_and_args({:when, _, [short_head | _]}) do
    name_and_args(short_head)
  end

  defp name_and_args(short_head) do
    Macro.decompose_call(short_head)
  end

  def set_code(state, code) do
    dex = Dex.set(state.dex, "res.code", code)
    %{state | dex: dex}
  end

  def set_body(state, body) when is_bitstring(body) do
    dex = Dex.set(state.dex, "res.body", body)
    %{state | dex: dex}
  end

  def set_body(state, body) do
    dex = Dex.set(state.dex, "res.body", to_string body)
    %{state | dex: dex}
  end

  def set_codebody(state, code, body) do
    set_code(state, code) |> set_body(body)
  end

  def to_integer(code) when not is_integer(code) do
    String.to_integer code
  end

  def to_integer(code), do: code

  def to_number!(str, default \\ 0) do
    cond do
      str in [nil, ""] -> default
      true ->
        String.contains?(str, ".") \
          && String.to_float(str) \
          || String.to_integer(str)
    end
  end

  defmodule AnalysisKV do
    defstruct curr_type: nil,
              curr_idx: 0,
              prefix: ""
  end
 
  def to_args(str, cnt \\ nil) do
    String.split(str, [", ", ",\n"], cnt && [parts: cnt] || [])
  end

  def prefixed_var(state, var) do
    if (local = local_id var, state) do
      "_local." <> local <> "." <> var
    else
      (local = List.first state.locals) \
        && "_local." <> local <> "." <> var
        || var
    end
  end

  def store(key, val, state) do
    if (local = local_id key, state) do
      store_in_local(local, key, val, state)
    else
      (local = List.first state.locals) \
        && store_in_local(local, key, val, state) \
        || %{state | dex: Dex.set(state.dex, key, val)}
    end
  end
 
  def args_to_map!(args) do
    for arg <- args, into: %{} do
      case String.split(arg, ":", parts: 2) do
        [key, val] -> {String.strip(key), String.lstrip(val)}
        [bad] -> throw {:bad_key, bad}
      end
    end
  end

  def store_in_local(id, name, val, state) do
    key = "_local." <> id <> "." <> name
    dex = Dex.set(state.dex, key, val)
    %{state | dex: dex}
  end

  def store_in_global(key, val, state) do
    dex = Dex.set(state.dex, key, val)
    %{state | dex: dex}
  end

  def local_id(name, state) do
    local_id(state.locals, name, state)
  end

  defp local_id([local | tl], name, state) do
    key = "_local." <> local <> "." <> name
    Dex.exist?(state.dex, key) && local \
      || local_id(tl, name, state)
  end

  defp local_id([], _name, _state), do: nil

  def erase(key, state) do
    if local = List.first state.locals do
      "_local." <> local <> "." <> key
    else
      key
    end
    |> erase_global(state)
  end

  def erase_global(key, state) do
    dex = Dex.unset(state.dex, key)
    %{state | dex: dex}
  end

  defp local_value([], _, _), do: nil
  defp local_value([local | tl], name, state) do
    Dex.val(state.dex, "_local." <> local <> "." <> name) \
      || local_value(tl, name, state)
  end

  defp global_value(key, state) do
    Dex.val(state.dex, key)
  end


  def value(key, state, default \\ nil) do
    key = case key do
      "?" <> key -> "req.params." <> key
      "#" <> key -> "req.headers." <> key
      _ -> key
    end
    local_value(state.locals, key, state) \
    || global_value(key, state)
    || default
  end
  
  def check_auth!(state) do
    state.req.__authorized === true && :ok || throw code: Code.unauthorized
  end

  def fun_node(state, id) do
    app = state.req.app
    app.__parsed[app.id <> "." <> id]
  end

  def fun_ref(mod, fun, arity) do
    {:&, [], [
        {:/, [context: Elixir, import: Kernel], [
            {
              {:., [], [
                  {:__aliases__, [alias: false], [mod]}, fun
              ]}, [], []
            }, arity
        ]}
    ]} |> Elixir.Code.eval_quoted |> elem 0
  end

  #
  # Tellet.Services.Auth
  #
  def node!("auth." <> fun, args) do
    fun = case fun do
      "basic"       -> :basic
      other         -> throw body: other
    end
    {Tellet.Services.Auth, fun, args}
  end

  #
  # Tellet.Services.Account
  #
  def node!("acc." <> fun, args) do
    fun = case fun do
      "new"         -> :new
      "get"         -> :get
      "put"         -> :put
      "del"         -> :del
      other         -> throw body: other
    end
    {Tellet.Services.Account, fun, args}
  end

  #
  # Tellet.Services.App
  #
  def node!("app." <> fun, args) do
    fun = case fun do
      "new"         -> :new
      "get"         -> :get
      "put"         -> :put
      "del"         -> :del
      other         -> throw body: other
    end
    {Tellet.Services.App, fun, args}
  end

  #
  # Tellet.Services.Message
  #
  def node!(fun, args) when
    fun in ["pub", "sub", "msg"]
  do
    {Tellet.Services.Message, List.to_existing_atom(fun), args}
  end

  #
  # Tellet.Services.Telephony
  #
  def node!("tel." <> fun, args) do
    {Tellet.Services.Telephony, List.to_existing_atom(fun), args}
  end

  #
  # Tellet.Services.Core
  #
  def node!(fun, args) when
    fun in ["get", "put", "del", "gets", "puts", "dels"]
  do
    {Tellet.Services.Data, List.to_existing_atom(fun), args}
  end

  def node!("on_" <> rest, args) do
    args = Map.put(args || %{}, "_id", rest)
    {Tellet.Services.Core, :on, args}
  end

  def node!(tag, args) do
    fun = case tag do
      "tellet"      -> :tellet
      "block"       -> :block
      "set"         -> :set
      "use"         -> :use_
      "case"        -> :case_
      "if"          -> :if_
      "else"        -> :else_
      "for"         -> :for_
      "raise"       -> :raise_
      "break"       -> :break
      "stop"        -> :stop
      "return"      -> :return
      "response"    -> :response
      "escape"      -> :escape
      "chunk"       -> :chunk
      "reply"       -> :response
      "switch"      -> :switch
      "sleep"       -> :sleep
      "note"        -> :note
      "authorized"  -> :authorized
      "stop-if"     -> :stop_if
      "break-if"    -> :break_if
      "return-if"   -> :return_if
      "escape-if"   -> :escape_if
      other         -> throw body: other
    end
    {Tellet.Services.Core, fun, args}
  end

end
