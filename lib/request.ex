defmodule Tellet.Request do

  @type sid :: bitstring
  @type token :: bitstring

  @type addr :: tuple
  @type port_no :: pos_integer
  @type peer :: {addr, port_no}
  @type source_pid :: pid

  defmodule Peer do
    defstruct ip: nil,
              port: nil,
              remote: nil
  end

  defstruct id: nil,
            user: nil,
            peer: nil,
            pid: nil,
            port: nil,
            host: nil,
            route: nil,
            path: nil,
            method: nil,
            headers: nil,
            body: nil,
            app: nil,
            params: nil,
            #cookies: nil,
            __authorized: false

  defmacro __using__(opts \\ []) do
    quote do
      alias unquote(__MODULE__), unquote(opts)
    end
  end # defmacro

  defmacro default_timeout, do: 60_000
end
