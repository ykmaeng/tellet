defmodule Tellet.XML do
  
  defmodule State do
    defstruct app_id: nil,
              graph: nil,
              group: nil,
              keys: %{},
              count: 0
  end

  defmacro __using__(opts) do
    quote do
      alias unquote(__MODULE__), unquote(opts)
    end
  end # defmacro

  import Logger
  use Dex.Digraph, as: Graph
  use Tellet.Code
  alias Tellet.Service

  def sax_parse!(app_id, xml) do
    g = Graph.new([:acyclic, :private])
    state = %State{app_id: app_id, graph: g, count: 0} 
    {:ok, node, _rest} = :erlsom.parse_sax(xml, state, &sax_event_handler/2)
    node
  end

  def sax_graph_to_node(graph, keys) do
    Enum.sort(keys) |> Enum.map fn(key) ->
      {mod, fun, args} = Graph.node(graph, key, :label)
      ref = Service.fun_ref(mod, fun, 2)
      keys = Graph.out_nodes(graph, key)
      {{mod, fun, ref, args}, sax_graph_to_node(graph, keys)}
    end
  end

  defp sax_event_handler({:startElement, _uri, elem, prefix, attrs}, state) do
    elem = to_string elem
    args = if attrs == [] do nil else
      Enum.into attrs, %{}, fn
      ({:attribute, name, ns, _, val}) ->
        key = (ns == '' && name || ns ++ ':' ++ name)
        preprocess_arg elem, {List.to_string(key), List.to_string(val)}
      end
    end
    {mod, fun, args} = Tellet.Service.node! elem, args
    :ok = apply(mod, fun, [{:check!, args}])
    node = {mod, fun, args}
    count = state.count + 1
    group = case fun do
      :tellet -> :main
      :on -> state.app_id <> "." <> (args["_id"] || throw code: Code.bad_request)
      _ -> state.group || throw Code.bad_request
    end
    keys = state.keys[group] || []
    Graph.put_node(state.graph, count, node)
    if (last_key = List.first keys) do
      Graph.new_edge(state.graph, last_key, count)
    else
      Graph.put_node(state.graph, group)
      Graph.new_edge(state.graph, :tellet, group)
      Graph.new_edge(state.graph, group, count)
    end
    keys = Map.put(state.keys, group, [count | keys])
    %{state | group: group, count: count, keys: keys}
  end

  defp sax_event_handler({:characters, chars}, state) do
    count = List.first state.keys[state.group]
    node = Graph.node(state.graph, count, :label)
    {mod, fun, args} = node
    chars = preprocess_args(fun, {"data", trim to_string(char)})
    args = Map.put(args || %{}, "data", chars)
    new_node = {mod, fun, args}
    Graph.put_node(state.graph, count, new_node)
    state
  end

  defp sax_event_handler({:endElement, _, elem, prefix}, state) do
    group = state.group
    [prev_key | tail] = state.keys[group]
    {mod, fun, args} = Graph.node(state.graph, prev_key, :label)
    case apply(mod, fun, [{:predef!, args, state}]) do
      {args, state} ->
        Graph.put_node(state.graph, prev_key, {mod, fun, args})
      nil ->
        nil
    end
    keys = Map.put(state.keys, group, tail)
    group = match?('on_' ++ _, elem) && :main || group
    %{state | group: group, keys: keys}
  end

  defp sax_event_handler(:startDocument, state) do
    Graph.put_node(state.graph, :tellet)
    state
  end

  defp sax_event_handler(:endDocument, state) do
    #sorted = Graph.sort_nodes(state.graph)
    Enum.into state.keys, %{}, fn({group, _}) ->
      nodes = Graph.out_nodes(state.graph, group)
      {group, sax_graph_to_node(state.graph, nodes) |> hd}
    end
  end

  defp sax_event_handler(_event, state) do
    state
  end

  defp sax_attr({:attribute, name, _, _, value}) do
    {List.to_string(name), List.to_string(value)} 
  end

  defp preprocess_arg _fun, {key, val} do
    #IO.inspect preprocess_arg: [fun: fun, key: key, val: val]
    {key, val} 
  end

  def trim(str) do
    """
    if str do
      replace(str, "><", ">[\s\r\n\t]+<")
      |> replace("", "^[\r\n\t\s]*|[\r\n\t\s]*$")
      |> replace("\s", "\\s")
      |> replace("\t", "\\t")
      |> replace("\r", "\\r")
      |> replace("\n", "\\n")
    else
      ""
    end
    """
    #regex = ~r/^[\r\n\t\s]+|[\r\n\t\s]+$/
    Regex.replace(~r/>[\s\r\n\t]+</, str, "><") |> String.strip
  end

  def replace(str, to \\ "", regex \\ "") do
	:re.replace(str, regex, to, [:global, :unicode, {:return, :binary}])
  end

end


