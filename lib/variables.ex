defmodule Tellet.Variables do

  defmacro __using__(opts) do
    quote do
      alias unquote(__MODULE__), unquote(opts)
    end
  end # defmacro

  defmodule State do
    defstruct dex: nil
  end

  use Tellet
  use Tellet.Server
  use Timex
  import Logger

  defstart start_link(opts \\ []) do
    :erlang.process_flag(:trap_exit, true)
    initial_state %State{dex: Dex.new}
  end

  defcall dex, state: state do
    reply state.dex
  end

  defcall put(_, Tellet.nilstr), do: reply nil
  defcall put(key, val), state: state do
    Dex.put(state.dex, key, val)
    |> reply
  end

  defcast delete(key), state: state do
    Dex.delete(state.dex, key)
    noreply
  end

  defcall eval!(str), state: state do
    try do
      Dex.JS.eval!(state.dex.js, str)
      |> reply
    catch
      :throw, error ->
        reply error
    end
  end

  def terminate(reason, state) do
    debug inspect [module: __MODULE__, fun: __ENV__.function, reason: reason]
    Dex.destroy(state.dex)
  end

end
