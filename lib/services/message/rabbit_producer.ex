defmodule Tellet.Services.Message.RabbitProducer do

  use Tellet.Server
  use AMQP
  import Logger
  alias Tellet.Services.Message.Rabbit

  defmodule State do
    defstruct chan: nil
  end

  @default_queue "noname"

  defstart start_link(args \\ []) do
    :erlang.process_flag(:trap_exit, true)
    debug inspect {__MODULE__, __ENV__.function, args: args}
    {:ok, chan} = Channel.open args[:conn]
    initial_state %State{chan: chan}
  end

  defcall pub(exchange, topic, body, opts), state: state do
    Basic.publish(state.chan, exchange, topic, body, opts)
    |> reply
  end

  defcast stop, do: stop_server(:normal)

  defhandleinfo ignored do
    warn inspect defhandleinfo: [ignored: ignored]
    noreply
  end

  def terminate(reason, state) do
    debug inspect {__MODULE__, __ENV__.function, reason: reason}
    close_channel_(state.chan)
  end

  defp close_channel_(channel) do
    Channel.close(channel)
  end

end


