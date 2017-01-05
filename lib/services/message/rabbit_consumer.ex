defmodule Tellet.Services.Message.RabbitConsumer do

  use Tellet.Server
  use AMQP
  import Logger
  alias Tellet.Services.Message.Rabbit

  defmodule State do
    defstruct chan: nil,
              funs: nil
  end

  defstart start_link(args \\ []) do
    :erlang.process_flag(:trap_exit, true)
    {:ok, chan} = Channel.open args[:conn]
    initial_state %State{chan: chan}
  end
  
  defcall chan, state: state do
    state.chan |> reply
  end

  defcall sub(queue, funs, opts \\ []), state: state do
    {:ok, tag} = Basic.consume(state.chan, queue, _pid = nil, [])
    %{state | funs: funs}
    |> set_and_reply {:ok, tag}
  end

  defcall ack(tag), state: state do
    Basic.ack(state.chan, tag)
    |> reply
  end

  defcast stop, do: stop_server(:normal)

  defhandleinfo {:basic_deliver, payload, props},
    state: state
  do
    IO.inspect payload: payload, props: props
    (fun = state.funs[:delivered]) && fun.(payload, props)
    noreply
  end

  defhandleinfo {:basic_consume_ok, %{consumer_tag: tag}},
    state: state
  do
    (fun = state.funs[:subscribed]) && fun.(tag)
    noreply
  end

  defhandleinfo {:basic_cancel, %{consumer_tag: tag}},
    state: state
  do
    (fun = state.funs[:cancel]) && fun.(tag)
    warn inspect {__MODULE__, __ENV__.function, :basic_cancel}
    stop_server :normal
  end

  defhandleinfo ignored do
    warn inspect {__MODULE__, :defhandleinfo, ignored: ignored}
    noreply
  end

  def terminate({{:shutdown, {reason, code, _}}, _}, _state) do
    warn inspect {__MODULE__, __ENV__.function, shutdown: reason, code: code}
    :ok
  end

  def terminate(reason, state) do
    debug inspect {__MODULE__, __ENV__.function, reason: reason}
    close_channel_(state.chan)
  end

  defp close_channel_(channel) do
    Channel.close(channel)
  end

end


