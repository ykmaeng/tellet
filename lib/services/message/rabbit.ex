defmodule Tellet.Services.Message.Rabbit do

  use Tellet.Server, name: __MODULE__
  use AMQP

  alias Tellet.Services.Message.RabbitConsumer, as: Consumer
  alias Tellet.Services.Message.RabbitFirehose, as: Firehose

  import Logger

  defmodule State do
    defstruct conn: nil,
              chan: nil
  end

  defstart start_link(args \\ []) do
    debug inspect {__MODULE__, __ENV__.function, args: args}
    {:ok, conn} = Connection.open args[:addr]
    {:ok, chan} = Channel.open(conn)
    #{:ok, _firehose} = Firehose.start_link(conn: conn)
    initial_state %State{conn: conn, chan: chan}
  end

  defcall conn, state: state do
    state.conn |> reply
  end

  defcall declare_exchange(name, type \\ :topic, opts \\ [durable: true]),
    state: state
  do
    Exchange.declare(state.chan, name, type, opts)
    |> reply
  end

  defcall declare_queue(name, opts \\ []),
    state: state
  do
    Queue.declare(state.chan, name, opts)
    |> reply
  end

  defcall bind(exchange, topic, queue, opts \\ []),
    state: state
  do
    opts = [{:routing_key, topic} | opts]
    Queue.bind(state.chan, queue, exchange, opts)
    |> reply
  end

  defcall unbind(exchange, topic, queue, opts \\ []),
    state: state
  do
    opts = [{:routing_key, topic} | opts]
    Queue.unbind(state.chan, queue, exchange, opts)
    |> reply
  end

  defcast stop, do: stop_server(:normal)

  defcall pub(exchange, topic, body, props \\ []), state: state do
    Basic.publish(state.chan, exchange, topic, body, props)
    |> reply
  end

  def transient_sub(queue, funs \\ nil, props \\ []) do
    conn = __MODULE__.conn
    {:ok, consumer} = Consumer.start_link conn: conn
    {:ok, tag} = Consumer.sub(consumer, queue, funs, props)
    {:ok, consumer, tag}
  end

  @man_port 15672
  defcall new_vhost(name) do
    url = "http://localhost:" <> @man_port <> "/api/vhosts/" <> name
  end

  defhandleinfo ignored do
    warn inspect {__MODULE__, :defhandleinfo, ignored: ignored}
    noreply
  end

  def terminate(reason, state) do
    debug inspect {__MODULE__, __ENV__.function, reason: reason}
    Channel.stop state.chan
    Connection.close state.conn
  end

  """
  defcall register_consumer(queue, exchange, bind \\ "#", callback \\ nil, opts \\ []),
    state: state
  do
    {:ok, chan} = Consumer.start_link(conn: state.conn)
    {:ok, _queue} = Queue.declare(chan, queue, opts)
    :ok = Consumer.bind(channel, exchange, bind)
    :ok = Consumer.sub(channel, name, callback, opts)
    reply {:ok, channel}
  end
  """

end

