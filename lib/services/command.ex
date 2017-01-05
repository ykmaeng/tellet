defmodule Tellet.Services.Command do

  defstruct fun: nil,
            args: [],
            data: nil
  
  defmacro __using__(opts) do
    quote do
      alias unquote(__MODULE__), unquote(opts)
    end
  end # defmacro

  @parse_regex ~r/^[$:]{1}(?:.|\s)+?(?=[,\s\n\t]*[\w-]+:[\s\n\t]+|$)/i
  @args_regex ~r/([\w-]+):[\s\n\t]+((?:.|\n)+?)(?=[,\s\n\t]*[\w-]+:[\s\n\t]+|$)/i

  def parse!(str) do
    String.split(str, "|")
    |> Enum.map fn str -> 
      str = String.strip(str) 
      args = args(str)
      case Regex.scan(@parse_regex, str) |> to_string do
        "$" <> var ->
          %__MODULE__{
            fun: :val,
            args: Map.put(args || %{}, "data", var)
          }
        ":" <> cmd -> 
          split = String.split(cmd, " ", parts: 2)
          %__MODULE__{
            fun: Enum.at(split, 0) |> String.to_existing_atom,
            args: Map.put(args || %{}, "data", Enum.at(split, 1))
          }
      end
    end
  end

  defp args(str) do
    for [_, key, val] <- Regex.scan(@args_regex, str), into: %{},
    do: {key, val}
  end

  defp do_eval("js", arg, state) do
    case Dex.js_eval!(arg) do
      {:error, reason} ->
        throw {:error, reason}
      val ->
        Dex.to_jstr(val)
    end
  end

  defp do_eval("length", arg, _state) do
    String.length(arg) |> to_string
  end

  defp do_eval("bytes", arg, _state) do
    byte_size(arg) |> to_string
  end

  defp do_eval("join", arg, _state) do
    to_string(to_args arg)
  end

  defp do_eval("json", arg, state) do
    case to_args(arg) do
      [] -> nil
      [val | args] ->
        key = (local = local_id val, state) \
          && "_local." <> local <> "." <> val \
          || val
        Dex.val(state.dex, key)
        |> do_eval_json_opts(args)
    end
    |> Dex.json!
  end

  defp do_eval("parse_" <> type, arg, state) do
    args = to_args arg
    var = prefixed_var(state, state.opts[:arg_key] || "val")
    case type do
      "json" ->
        json = Tellet.Service.value(List.first(args), state)
        Dex.set state.dex, var, Dex.JSON.decode!(json)
      "body" ->
        map = http_body_to_map List.first(args)
        Dex.set state.dex, var, map
    end
    nilstr
  end

  defp do_eval("split", arg, state) do
    do_eval_split(to_args(arg), state)
    nilstr
  end

  defp do_eval("uri." <> cmd, arg, state) do
    case cmd do
      "split" ->
        do_eval_split(to_args(arg), state, fn(val) -> URI.decode(val) end)
      "encode" ->
        nil
    end
    nilstr
  end

  defp do_eval("time." <> cmd, arg, _state) do
    args = to_args(arg)
    date = Date.universal
    case cmd do
      "now" ->
        case List.first(args) do
          "usecs" -> Time.now(:usecs) |> trunc
          "msecs" -> Time.now(:msecs) |> trunc
          "secs" -> Time.now(:secs) |> trunc
          "mins" -> Time.now(:mins) |> trunc
          "hours" -> Time.now(:hours) |> trunc
          _ -> DateFormat.format!(date, "{RFC1123}")
        end
      "weekday" -> Date.weekday(date)
      "days_in_month" -> Date.days_in_month(date)
      _ ->
        ""
    end
    |> to_string
  end

  defp do_eval("math." <> cmd, arg, state) do
    args = to_args(arg)
    case String.to_existing_atom(cmd) do
      :pi -> :math.pi |> truncate_float(List.first(args) || "") |> to_string
      :sin -> do_eval_math(&:math.sin/1, args)
      :cos -> do_eval_math(&:math.cos/1, args)
      :tan -> do_eval_math(&:math.tan/1, args)
      :asin -> do_eval_math(&:math.asin/1, args)
      :acos -> do_eval_math(&:math.acos/1, args)
      :atan -> do_eval_math(&:math.atan/1, args)
      :sinh -> do_eval_math(&:math.sinh/1, args)
      :cosh -> do_eval_math(&:math.cosh/1, args)
      :tanh -> do_eval_math(&:math.tanh/1, args)
      :asinh -> do_eval_math(&:math.asinh/1, args)
      :acosh -> do_eval_math(&:math.acosh/1, args)
      :atanh -> do_eval_math(&:math.atanh/1, args)
      :exp -> do_eval_math(&:math.exp/1, args)
      :log -> do_eval_math(&:math.log/1, args)
      :log10 -> do_eval_math(&:math.log10/1, args)
      :sqrt -> do_eval_math(&:math.sqrt/1, args)
      :pow -> case args do
        [x, y | opt] ->
          :math.pow(to_number!(x), to_number!(y))
          |> truncate_float(List.first(opt) || "") |> to_string
        _ ->
          ""
      end
      other -> do_eval_math(other, args, state)
    end # case
  end
 
  defp do_eval(_, _, _state), do: ""

  defp do_eval_json_opts(map, []), do: map

  defp do_eval_json_opts(map, ["strip" <> rest | tl])
    when is_map(map)
  do
    cnt = case rest do
      "" -> 1
      ": " <> n -> to_number! n
    end
    Enum.reduce(1..cnt, map, fn
      (_, acc) when is_map(acc) ->
        case Enum.at acc, 0 do 
          {_key, val} -> val
          nil -> acc
        end
      (_, acc) ->
        acc
    end)
    |> do_eval_json_opts(tl)
  end

  defp do_eval_json_opts(val, ["wrap: " <> title | tl]) do
    Map.put(%{}, title, val) |> do_eval_json_opts(tl)
  end

  defp do_eval_json_opts(val, [_ | tl]) do
    do_eval_json_opts(val, tl)
  end

  defp http_body_to_map(nil), do: nil
  defp http_body_to_map(""), do: ""
  defp http_body_to_map(body) do
    Enum.reduce String.split(body, "&"), %{}, fn(kv, map) ->
      case String.split(kv, "=") do
        [key, val] -> Map.put(map, key, val)
        _ -> map
      end
    end
  end
  
  defp do_eval_split(args, state, fun \\ nil) do
    var = Tellet.Service.value("__key", state, "res.body")
    {splits, map_vars} = case args do
      [] -> {[], []}
      [str] -> {String.split(str), []}
      [str, pattern | map_vars] -> {String.split(str, pattern), map_vars}
    end
    Enum.reduce splits, 0, fn(val, cnt) ->
      val = fun && fun.(val) || val
      mapvar = Tellet.Service.at_list(map_vars, cnt)
      state = mapvar && Tellet.Service.store(mapvar, val, state) || state
      state = map_vars == [] && Tellet.Service.store(Tellet.Service.make_list(var, cnt), val, state) || state
      cnt + 1
    end
  end

  defp do_eval_math(fun, args) do
    [val | opt] = args
    fun.(to_number! val) |> truncate_float(List.first(opt) || "") |> to_string
  end

  defp do_eval_math(:eval, args, _state) do
    stmt = (Enum.into args, [], fn(x) ->
      (x == nil or x == "") && "0" || x
    end) |> Enum.join
    Regex.match?(~r/[a-zA-Z]/, stmt) && throw body: "invalid format"
    Elixir.Code.eval_string(stmt) |> elem(0) |> to_string
  end

  defp do_eval_math(_, _, _), do: ""

  defp truncate_float(f, "c" <> n), do:
    Float.ceil(f, n == "" && 0 || max15(String.to_integer n))
  defp truncate_float(f, "f" <> n), do:
    Float.floor(f, n == "" && 0 || max15(String.to_integer n))
  defp truncate_float(f, "r" <> n), do:
    Float.round(f, n == "" && 0 || max15(String.to_integer n))
  defp truncate_float(f, _), do: f

  defp max15(num) when num > 15, do: 15
  defp max15(num), do: num

end

