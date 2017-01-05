defmodule Tellet.Data.Adapters.Riak do

  use Tellet.Server, name: __MODULE__
  import Logger

  defmodule State do
    defstruct conn: nil
  end

  defstart start_link(args \\ []) do
    {:ok, conn} = :riakc_pb_socket.start_link(args[:host], args[:port])
    initial_state(%State{conn: conn})
  end

  defcall echo(msg) do
    reply msg
  end

  defcall put(bucket, key, val), state: state do
    obj = :riakc_obj.new(bucket, key, val)
    reply :riakc_pb_socket.put(state.conn, obj)
  end

  defcall get(bucket, key), state: state do
    case :riakc_pb_socket.get(state.conn, bucket, key) do
      {:ok, obj} -> reply {:ok, :riakc_obj.get_value(obj)}
      error -> reply error
    end
  end

  defcall delete(bucket, key), state: state do
    reply :riakc_pb_socket.delete(state.conn, bucket, key)
  end

  defcall stop, state: state do
    {:stop, :normal, state}
  end

  def terminate(reason, _state) do
    IO.puts "terminate -> reason: #{inspect reason}"
  end

end

