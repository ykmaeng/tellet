defmodule Tellet.Services.Message.RabbitFirehose do

  use Tellet.Server, name: __MODULE__
  use AMQP

  alias Tellet.Services.Message.Rabbit
  alias Tellet.Services.Message.RabbitConsumer, as: Consumer

  import Logger

  defmodule State do
    defstruct conn: nil,
              consumer: nil,
              tag: nil

  end

  @exchange "amq.rabbitmq.trace"
  @topic "#"
  @queue "firehose"

  defstart start_link(args \\ []) do
    debug inspect {__MODULE__, :start_link, args: args}
    conn = args[:conn] || throw "rabbitmq connection is null"
    {:ok, consumer} = Consumer.start_link(conn: conn)
    :ok = ensure_queue_bind(consumer, @exchange, @topic, @queue)
    {:ok, tag} = sub(consumer, @queue)
    initial_state %State{
      conn: conn, consumer: consumer, tag: tag
    }
  end

  defcast callback(payload, props), state: state do
    IO.inspect {__MODULE__, :callback, payload: payload, props: props}
    noreply
  end

  defp sub(consumer, queue) do
    from = self
    {:ok, tag} = Consumer.sub(consumer, queue,
      [ # callbacks
        delivered: fn(payload, props) ->
          __MODULE__.callback(payload, props)
        end
      ]
    )
    {:ok, tag}
  end

  defp ensure_queue_bind(consumer, exchange, topic, queue) do
    chan = Consumer.chan(consumer)
    {:ok,
      %{consumer_count: _,
        message_count: _,
        queue: _}
    } = Queue.declare(chan, queue, durable: true)
    Queue.bind(chan, queue, exchange, routing_key: topic)
  end

  def terminate(reason, state) do
    debug inspect {__MODULE__, __ENV__.function, reason: reason}
    Channel.stop state.chan
    Connection.close state.conn
  end

end


