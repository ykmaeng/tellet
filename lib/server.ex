defmodule Tellet.Server do

  # available options: name, state, timeout
  defmacro __using__(opts \\ []) do
    keychange = fn (props, from, to) ->
      List.keyreplace(props, from, 0, {to, props[from]})
    end

    opts = opts[:name] &&
      keychange.(opts, :name, :export) ||
      opts #[{:export, __CALLER__.module} | opts]

    quote do
      use ExActor.GenServer, unquote(opts)
    end
  end

end

