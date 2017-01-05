defmodule Tellet.Worker do

  defmodule State do

    defstruct req: %Tellet.Request{},
              res: %Tellet.Response{},
              dex: nil,
              locals: [],
              node: %{
                current: nil,
                children: nil,
                tail: [],
                hops: 0
              },
              opts: nil

  end

  use Tellet.Server
  use Tellet.Code
  use Tellet.XML
  use Tellet.Response
  use Tellet.Service
  use Tellet.Services.Core
  import Logger

  def begin(req) do
    {:ok, worker} = start_link(req: req)
    execute worker
    await req.id
  end

  defp await(id, timeout \\ :infinity) do
    receive do
      {^id, res} -> res
    after timeout ->
      response(:timeout)
    end
  end

  defstart start_link(opts)
    #gen_server_opts: [timeout: 10_000]
  do
    req = opts[:req]
    state = %State{
      req: req,
      dex: Dex.new
    }
    initial_state state
  end

  defcastp execute, state: state do
    try do
      state = set_variables(state)
      route([state.req.app.__parsed.main], [], state)
      |> response |> __MODULE__.reply(state)
    rescue
      exception ->
        #msg = Exception.message(exception)
        handle_throw(state, body: inspect exception)
    catch
      :throw, msg ->
        handle_throw(state, msg)
    end
    stop_server :normal
  end

  defp handle_throw(state, {error, reason}) do
    reason = cond do
      is_bitstring(reason) -> reason
      is_number(reason) -> to_string reason
      true -> inspect reason
    end
    msg = to_string(error) <> ": " <> reason
    res = %Response{
      code: Code.bad_request,
      body: msg
    }
    response({:error, %{state | res: res}}) |> __MODULE__.reply(state)
    print_error msg
  end

  defp handle_throw(state, msg) when is_list(msg) do
    res = %Response{
      code: msg[:code] || Code.bad_request,
      body: msg[:body],
      headers: msg[:headers]
    }
    response({:error, %{state | res: res}}) |> __MODULE__.reply(state)
    print_warn msg[:body]
  end

  defp handle_throw(state, error) when is_atom(error) do
    handle_throw(state, {:error, error})
  end

  defp print_warn(msg) do
    warn inspect message: msg, trace: System.stacktrace
  end

  defp print_error(msg) do
    error inspect message: msg, trace: System.stacktrace
  end

  defp set_variables(state) do
    dex = state.dex
    req = state.req
    dex = Dex.set(dex, "req.path", req.path)
      |> Dex.set("req.route", req.route)
      |> Dex.set("req.body", req.body)
      |> Dex.set("req.params", req.params)
      |> Dex.set("req.method", req.method)
      |> Dex.set("req.headers", req.headers)
      |> Dex.set("req.peer", Map.from_struct(req.peer))
    %{state | dex: dex} 
  end

  def route([{current, children} | tail] = nodes, rest, state) do
    {_, fun, ref, args} = current
    node = %{state.node |
      current: current,
      children: children,
      tail: tail,
      hops: state.node.hops + 1
    }
    state = %{state | node: node}
    if valid_state?(state) do
      args = transform_args(fun, args, state)
      case work(ref, [args, state]) do
        {:enter, state} -> route(state.node.children, state.node.tail++rest, state)
        {:next, state} -> route(state.node.tail, rest, state)
        {:repeat, state} -> route(nodes, rest, state)
        {:escape, state} -> route(rest, [], state)
        {:break, state} -> {:break, state}
        {:stop, state} -> {:stop, state}
        {:error, state} -> {:error, state}
      end
    else
      {:error, state}
    end
  end

  def route([], [], state), do: {:ok, state}
  def route([], [hd | tl], state), do: route([hd], tl, state)

  defp work(fun_ref, [args, state]) do
    fun_ref.(args, state)
  end

  defp transform_args(_, nil, _state), do: nil
  defp transform_args(:set, args, state) do
    var = args["var"]; val = args["data"]
    if (var && val) do
      var = Core.compile_template(var, state)
      args = Map.put(%{}, var, val)
    end
    transform_args(nil, args, state)
  end

  defp transform_args(_, args, state) do
    Enum.into args, %{}, fn
      {key, val} when is_bitstring(val) ->
        state = %{state | opts: [arg_key: key]}
        {key, Core.compile_template(val, state)}
      {key, val} ->
        {key, val}
    end
  end

  defp valid_state?(state) do
    (state.node.hops <= max_hops) || throw body: "exceeded maximum hops (#{max_hops})"
  end

  defp response({:ok, state}) do
    dex = state.dex
    if state.res do
      code = state.res.code \
        || Dex.val(dex, "res.code") \
        || Code.ok
      body = state.res.body \
        || Dex.val(dex, "res.body") \
        || ""
      headers = Dex.val(dex, "res.headers")
      %{state.res | code: code, body: body, headers: headers}
    else
      %Response{}
    end
  end

  defp response({:break, state}), do: response({:ok, state})
  defp response({:stop, state}), do: response({:ok, state})
  defp response({:error, state}), do: state.res

  defp response(:timeout) do
    %Response{code: Code.request_timeout}
  end

  def reply(res, state) do
    send state.req.pid, {state.req.id, res}
  end

  def terminate(reason, state) do
    debug inspect [module: __MODULE__, fun: __ENV__.function, reason: reason]
  end
end

