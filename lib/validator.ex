defmodule Tellet.Validator do

  use Tellet.Code, as: Code

  defmacro __using__(opts) do
    quote do
      unquote(if opts[:as] do
        quote do alias unquote(__MODULE__), as: unquote(opts[:as]) end
      end)
    end
  end # defmacro

  def check(args, conditions), do: check("", args, conditions)

  def check(fun, args, conditions) do
    res = Enum.reject conditions, fn({name, expr}) ->
      apply(__MODULE__, name, [args, expr])
    end
    res == [] && :ok || [{:invalid, to_string(fun)} | res]
  end

  def check!(fun, args, conditions) do
    res = check(fun, args, conditions)
    res == :ok && res || throw body: (inspect res)
  end

  def count(args, fixed) when is_number(fixed), do: Map.size(args || %{}) == fixed
  def count(args, range), do: Map.size(args || %{}) in range

  def available([{key, _val} | tl], conds) when is_bitstring(key),
    do: :lists.member(key, conds) && available(tl, conds)
  def available([{key, _val} | tl], conds) when not is_bitstring(key),
    do: available(tl, conds)
  def available([], _conds), do: true
  def available(args, conds), do: available(Map.to_list(args || %{}), conds)

  def required(args, one) when is_bitstring(one), do: required(args, [one])
  def required(args, [hd | tl]), do: args[hd] && required(args, tl)
  def required(_args, []), do: true

  def exist_any(args, [hd | tl]), do: args[hd] || exist_any(args, tl)
  def exist_any(_args, []), do: false

  def exist_one(args, conds), do: exist_one(args, conds, 0)
  def exist_one(_args, [], acc), do: acc == 1
  def exist_one(args, [hd | tl], acc) do
    exist_one(args, tl, args[hd] && (acc + 1) || acc)
  end

  def match(args, [{name, re} | tl]) do
    String.match?(args[name] || "", re) && match(args, tl)
  end
  def match(_args, []), do: true
  def match(args, one) when is_tuple(one), do: match(args, [one])
  def match(args, re) do
    if Regex.regex?(re) do
      (Enum.drop_while args, fn({_key, val}) ->
        String.match? val, re
      end) == []
    end || false
  end

  def match_any(_args, {_name, []}), do: false
  def match_any(args, {name, [re | tl]}) do
    String.match?(args[name] || "", re) || match_any(args, {name, tl})
  end

  def match_one(args, cond), do: match_one(args, cond, 0)
  def match_one(_args, {_name, []}, acc), do: acc == 1
  def match_one(args, {name, [re | tl]}, acc) do
    match_one(args, {name, tl}, String.match?(args[name] || "", re) && (acc + 1) || acc)
  end

end
