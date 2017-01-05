defmodule Tellet.App do

  #@derive [Access]
  defstruct id: nil,
            owner: nil,
            revision: 0,
            plain: nil,
            static: false,
            content_type: nil,
            md5: nil,
            __parsed: nil,
            enabled: true,
            created: 0

  @type t :: %Tellet.App{
    id: String.t,
    owner: String.t,
    revision: 0 | pos_integer,
    plain: String.t,
    static: boolean,
    content_type: String.t,
    __parsed: map,
    created: 0 | pos_integer,
    enabled: boolean
  }

  @bucket __MODULE__

  alias Tellet.Data
  alias Tellet.XML
  import Logger

  def get(_, "_" <> app_id) do
    get(nil, app_id)
  end

  def get(user_id, app_id) do
    key = key(user_id, app_id)
    case Data.get(@bucket, key) do
      {:error, _} ->
        :app_notfound
      app ->
        app = :erlang.binary_to_term(app)
        {:ok, app}
    end
  end

  defp key(user_id, app_id) do
    (user_id || "") <> "/" <> app_id
  end

  def exist?(user_id, app_id) do
    get(user_id, app_id) != :app_notfound
  end

  def new(user_id, app_id, body, userdata \\ nil) do
    if exist?(user_id, app_id) do
      :app_already_exists
    else
      put(user_id, app_id, body, userdata)
    end
  end

  def put(user_id, app_id, body, userdata \\ nil) do
    parsed = XML.sax_parse!(app_id, body)
    app = %Tellet.App{
      id: app_id,
      owner: user_id,
      plain: body,
      __parsed: parsed,
      created: :tellet_util.now_sec
    }
    :ok = Data.persist(@bucket, key(user_id, app_id), app)
  end

  def del(user_id, app_id) do
    key = key(user_id, app_id)
    Data.delete(@bucket, key)
  end

  def check!([{current, children} | tail], rest, state) do
    {mod, fun, _, args} = current
    state = %{state | children: children, tail: tail}
    :ok = apply(mod, fun, [{:check!, args}])
    {args, state} = apply(mod, fun, [{:prepare!, args, state}])
    check!(state.children, state.tail ++ rest, state)
  end

  def check!([], [], state), do: {:ok, state}
  def check!([], [hd | tl], state), do: check!([hd], tl, state)
end
