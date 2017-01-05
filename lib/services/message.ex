defmodule Tellet.Services.Message do

  use Tellet.Service
  import Logger
  alias __MODULE__.Rabbit
  alias __MODULE__.RabbitConsumer, as: Consumer

  @bucket __MODULE__
  @vhost Tellet.conf(__MODULE__, :vhost)
  @default_sub_count 10
  @default_sub_timeout 60_000
  @default_topic "noname"
  @default_queue_expires (3600_000*24)
  @default_queue_opts [
    #durable: true,
    #exclusive: true,
    arguments: [{"x-expires", :signedint, @default_queue_expires}]
  ]

  defunp register_queue args, state,
    available: ["name", "bind",
                "callback.app", "callback.fun",
                "policy.x-expires", "policy.x-message-ttl"],
    required: ["name", "bind"]
  do
    ex = Rabbit.declare_exchange(state.user.id)
    queue = case args["name"] do
      "" -> "noname"
      name -> name
    end
    {:ok, _} = Rabbit.declare_queue(queue)
    :ok = Rabbit.bind(ex, args["bind"])
    next! state
  end

  defunp pub args, state do
    if (body = args["body"]) do
      user_id = state.req.user.id
      ex = exchange(user_id)
      topic = (topic args["topic"]) || @default_topic
      :ok = Rabbit.pub(ex, topic, body, [
        app_id: args["app"], headers: []
      ])
    end
    next! state
  end

  defp ensure_queue_bind(exchange, topic, queue) do
    {:ok,
      %{consumer_count: _,
        message_count: _,
        queue: _}
    } = Rabbit.declare_queue(queue, @default_queue_opts)
    Rabbit.bind(exchange, topic, queue)
  end

  defunp sub args, state do
    from = self
    exchange = exchange(state.req.user.id)
    topic = (topic args["topic"]) || @default_topic
    queue = (queue args["queue"]) || topic
    timeout = sub_timeout args["timeout"]
    :ok = ensure_queue_bind(exchange, topic, queue)
    {:ok, consumer, tag} = Rabbit.transient_sub(
      queue,
      [ # callbacks
        delivered: fn(payload, props) ->
         send from, {props.consumer_tag, payload, props}
        end
      ],
      [ # options
        count: to_number!(args["count"], @default_sub_count)
      ]
    )
    handle_sub(:receive, {consumer, tag, _cnt=0, [], timeout, state})
    next! state
  end

  defp exchange(user_id) when :erlang.size(user_id) > 0, do: "msg." <> user_id
  defp exchange(invalid), do: throw body: "invalid exchange name: " <> invalid

  defp queue(str) when :erlang.size(str) > 0, do: str
  defp queue(_), do: nil

  defp topic(str) when :erlang.size(str) > 0, do: str
  defp topic(_), do: nil

  defp sub_timeout(str) when :erlang.size(str) > 0, do: (to_number! str) * 1000
  defp sub_timeout(_), do: @default_sub_timeout

  def handle_sub(:receive, {consumer, tag, cnt, delivery_tags, timeout, state}) do
    receive do
      {tag, payload, props} ->
        #Consumer.ack(consumer, 
        name = "val:" <> to_string(cnt)
        store(state, name <> ".topic", props.routing_key)
        case props.headers do
          headers when is_list(headers) ->
            for {key, _, val} <- props.headers do
              store(state, name <> ".headers." <> key, val)
            end
          _ ->
            nil
        end
        store(state, name <> ".body", payload)
        handle_sub(:receive, {
          consumer, tag, cnt+1, [props.delivery_tag | delivery_tags], 1, state
        })
    after
      timeout ->
        if cnt > 0 do
          for tag <- delivery_tags, do: Consumer.ack(consumer, tag)
        else
          store(state, "val", [])
        end
        Consumer.stop(consumer)
    end
  end

end

