defmodule Dex.Service.Plugins.Core do

  use Tellet
  use Tellet.Code
  use Tellet.XML
  use Tellet.Service
  use Timex
  import Kernel
  #import Logger

  defmacro __using__(opts) do
    quote do
      alias unquote(__MODULE__), unquote(opts)
    end
  end # defmacro


  defun tellet args, state do
    if (body = args["data"]) do
      put_in(state.res.body, body)
      |> next! 
    else
      method = String.downcase state.req.method
      children = if fun_node(state, method) do
        params = Dex.val(state.dex, "req.params")
        args = Map.put(params || %{}, "on", method)
        {mod, fun, args} = node!("raise", args)
        ref = fun_ref(mod, fun, 2)
        [{{mod, fun, ref, args}, []} | state.node.children]
      else
        state.node.children
      end
      if fun_node(state, "init") do
        {mod, fun, args} = node!("raise", %{"on" => "init"})
        ref = fun_ref(mod, fun, 2)
        children = [{{mod, fun, ref, args}, []} | children]
      end
      put_in(state.node.children, children)
      |> enter! 
    end
  end

  defun authorized _args, state do
    put_in(state.req.__authorized, true)
    |> next! 
  end
  
  defun return args, state do
    val = args["data"] || value("data", state)
    store_in_global("data", val, state)
    |> escape!
  end

  defun response args, state do
    args = args || %{}
    {code, args} = Map.pop(args, "code", value("res.code", state))
    {body, args} = Map.pop(args, "data", value("res.body", state))
    code = code && to_integer(code) || Code.ok
    headers = Enum.into(
      Dex.val(state.dex, "res.headers", %{}),
      args
    )
    res = state.res \
      && %{state.res | code: code, body: body, headers: headers} \
      || %Tellet.Response{}
    Tellet.Worker.reply(res, state)
    next! %{state | res: nil}
  end

  defun use_ args, state do
    app = args["app"]
    case app do
      nil -> next! state
      "" -> next! state
      "id:" <> _ -> do_use_app(:id, app, args, state)
      "http:" <> _ -> do_use_app(:http, app, args, state)
      "https:" <> _ -> do_use_app(:http, app, args, state)
      _ -> do_use_app(:plain, app, args, state)
    end
  end

  defp do_use_app(:plain, app, _args, state) do
    parsed = XML.sax_parse!(state.req.app.id, app)
    new_parsed = Map.merge state.req.app.__parsed, Map.delete(parsed, :main)
    req = %{state.req | app: %{state.req.app | __parsed: new_parsed}}
    node = %{state.node | children: [parsed.main]}
    enter! %{state | req: req, node: node}
  end

  defp do_use_app(_, _app, _args, state) do
    next! state
  end

  defun set args, state do
    (Enum.reduce (args || []), state, fn
      {key, val}, acc when val != nilstr ->
        val = do_expr! val, state
        store(key, val, state)
      _, acc ->
        acc
    end)
    |> next!
  end

  defun unset args, state do
    (Enum.reduce (args || []), state, fn {key, _} ->
      erase(key, state)
    end)
    |> next! 
  end

  defun note _args, state do
    next! state
  end

  defun break_if args, state do
    exit_if args, state, &(break! &1)
  end

  defun stop_if args, state do
    exit_if args, state, &(stop! &1)
  end

  defun return_if args, state do
    exit_if args, state, &(escape! &1)
  end

  defun escape_if args, state do
    exit_if args, state, &(escape! &1)
  end

  defp exit_if(args, state, action) do
    {val, args} = Map.pop(args, "data")
    if satisfied?(args, state) == [] && val do
        store_in_global("data", val, state)
        |> action.()
    else
      next! state
    end
  end

  defun if_ args, state do
    (Enum.split_while state.node.children, fn {elem, _} ->
      not match? {_, :else_, _, _}, elem
    end)
    |> do_if(args, state)
  end

  defp do_if({true_children, _}, nil, state) do
    enter! put_in(state.node.children, true_children)
  end

  defp do_if({true_children, else_children}, args, state) do
    if (satisfied? args, state) == [] do
      enter! put_in(state.node.children, true_children)
    else
      enter! put_in(state.node.children, else_children)
    end
  end

  defun else_ _args, state, count: 0 do
    next! state
  end

  defun case_ args, state do
    do_case(args, state)
  end

  defp do_case(nil, state) do
    enter! put_in(state.node.tail, [])
  end

  defp do_case(args, state) do
    continue? = args["break"] == "no"
    if Map.delete(args, "break") |> satisfied?(state) == [] do
      continue? \
        && enter!(state) \
        || enter!(%{state | node: %{state.node | tail: []}})
    else
      next! state
    end
  end

  defun switch _args, state do
    enter! state
  end

  defun raise_ args, state,
    required: ["on"]
  do
    on = args["on"]
    if fun_node = fun_node(state, on) do
      {{mod, fun, ref, local_args}, children} = fun_node
      local_args = Map.merge local_args, (args || %{})
      unset_ref = fun_ref(__MODULE__, :unset_local, 2)
      exec_args = %{mod: __MODULE__, fun: :unset_local, ref: unset_ref, id: on}
      children = [
        {{mod, fun, ref, local_args}, children},
        {{__MODULE__, :exec, fun_ref(__MODULE__, :exec, 2), exec_args}, []}
      ]
      enter! put_in(state.node.children, children)
    else
      next! state
    end
  end

  defp set_local(local_id, local_args, state) do
    state = %{state | locals: [local_id | state.locals]}
    set_local_data(local_id, local_args, state)
  end

  defp set_local_data(id, args, state) do
    Enum.reduce args || [], state, fn {key, val}, acc ->
      if is_bitstring(key) do
        store_in_local(id, key, val, acc)
      else
        acc
      end
    end
  end

  defun exec args, state do
    state = args.ref.(args.id, state)
    next! state
  end

  def unset_local(id, state) do
    [local | tail] = state.locals
    id == local || throw [
      code: Code.bad_request,
      body: "invalid local! id: #{id}, local: #{local}"
    ]
    state = erase_global("_local." <> local, state)
    %{state | locals: tail}
  end

  defun for_ args, state, count: 1 do
    {var, enum} = Enum.at(args, 0)
    enum = case String.split(enum, "..") do
      [from, to] ->
        (to_number! from)..(to_number! to)
      _object ->
        (Dex.keys(state.dex, args[var]) |> Enum.sort) || []
    end
    local_id = to_string state.node.hops
    state = set_local(local_id, args, state)
    {mod, fun, args} = node!("block", nil)
    ref = fun_ref(mod, fun, 2)
    looping_nodes = [{{mod, fun, ref, args}, state.node.children}]
    dex = state.dex
    try do
      Enum.reduce enum, state, fn(x, acc) ->
        acc = if is_number(x) do
          store(var, x, acc)
        else
          state = store(var <> ".key", x, acc)
          store(var <> ".val", Dex.val(dex, x), state)
        end
        case Tellet.Worker.route(looping_nodes, [], acc) do
          {:ok, state_} -> state_
          {:break, _state_} -> throw :break
          {:stop, _state_} -> throw :stop
          {:error, _state_} -> throw body: "<for/> error"
        end
      end
    rescue
      ex -> raise ex
    catch
      :throw, :break -> next! unset_local(local_id, state)
      :throw, :stop -> stop! state
      :throw, error -> throw error
    else
      _state_ -> next! unset_local(local_id, state)
    end
  end
  
  defun sleep args, state do
    ms = args["ms"]
    ms && (String.to_integer(ms) |> :timer.sleep)
    next! state
  end

  defun map _args, state do
    next! state
  end

  defun reduce _args, state do
    next! state
  end

  defun template _args, state do
    next! state
  end

  defun repeat _args, state do
    repeat! state
  end

  predef on args, state do
    {args, state}
  end

  defun on args, state, required: "_id" do
    enter! set_local(args["_id"], args, state)
  end

  defun block _args, state do
    enter! state
  end

  defun escape args, state do
    exit_with_val args, state, &(escape! &1)
  end

  defun break args, state do
    exit_with_val args, state, &(break! &1)
  end

  defun stop args, state do
    exit_with_val args, state, &(stop! &1)
  end

  defp exit_with_val(args, state, action) do
    (val = args["data"]) && store_in_global("data", val, state) || state
    |> action.()
  end

  defp satisfied?(args, state) do
    Enum.drop_while args, fn({k, v}) ->
      left = value(k, state)
      if left do
        left = to_string left
        case String.split(v, " ", parts: 2) do
          ["not " <> right] -> !satisfied?("eq", left, right)
          [right] -> satisfied?("eq", left, right)
          ["not " <> cmd, right] -> !satisfied?(cmd, left, right)
          [cmd, right] -> satisfied?(cmd, left, right)
        end
      else
        false
      end # if
    end # Enum
  end
  
  defp satisfied?("re", left, right) do
    case Regex.compile(right) do
      {:ok, regex} -> String.match? left, regex
      {:error, _reason} -> false
    end
  end

  defp satisfied?("gt", left, right) do
    {true?, _} = Elixir.Code.eval_string(left <> ">" <> right); true?
  end

  defp satisfied?("lt", left, right) do
    {true?, _} = Elixir.Code.eval_string(left <> "<" <> right); true?
  end

  defp satisfied?("eq", left, right) do
    left == right
  end

  defp satisfied?("ne", left, right) do
    left != right
  end

  defp satisfied?("ge", left, right) do
    {true?, _} = Elixir.Code.eval_string(left <> ">=" <> right); true?
  end

  defp satisfied?("le", left, right) do
    {true?, _} = Elixir.Code.eval_string(left <> "<=" <> right); true?
  end

  defp satisfied?("in", left, right) do
    if String.contains?(right, "..") do
      {true?, _} = Elixir.Code.eval_string(left <> " in " <> right); true?
    else
      left in String.split(right, [",", ", "])
    end
  end

  defp satisfied?(cmd, left, right) do
    left == (cmd <> " " <> right)
  end

  def compile_template(str, state) do
    regex = ~r/\{\{[\s\t\n]*([=:]{1}(?:.|\n)+?)[\s\n\t]*}}/
    Regex.replace regex, str, fn(matched, expr) ->
      do_expr!(expr, matched, state)
    end
  end

  def do_expr!(expr, state) do
    do_expr!(expr, expr, state)
  end

  def do_expr!(expr, not_matched, state) do
    (case String.strip(expr) |> String.split(["\s", "\t", "\n"], parts: 2) do
      [":" <> cmd] ->
        String.rstrip(cmd) #|> evaluate("", state)
      [":" <> cmd, arg] ->
        arg = do_expr! arg, state
        String.rstrip(cmd) #|> evaluate(arg, state)
      ["$" <> var | args] ->
        case Dex.to_jstr(value var, state) do
          "\"" <> str -> String.slice(str, 0..-2)
          nil -> List.first args
          str -> str
        end
      _ when is_function(not_matched) ->
        not_matched && not_matched.()
      _ ->
        not_matched
    end || "") |> to_string
  end

  defp make_list(str, n) do
    str <> ":" <> to_string(n)
  end

  defp at_list(nil, _), do: nil
  defp at_list([], _), do: nil
  defp at_list(list, nth), do: list && Enum.at(list, nth)

end
