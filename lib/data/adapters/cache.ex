defmodule Tellet.Data.Adapters.Cache do

  use Tellet.Server, name: __MODULE__

  defmodule State do
    defstruct buckets: []
  end

  defstart start_link(args \\ []) do
    initial_state(%State{})
  end

  defcall new_bucket(bucket), state: state do
    case start_child(bucket) do
      {:ok, pid} ->
        state = %{state | buckets: [bucket | state.buckets]}
        {:reply, {:ok, pid}, state}
      error ->
        {:reply, error, state}
    end
  end

  defcall buckets, state: state do
    reply state.buckets
  end

  defcast expired(bucket, key) do
    reply delete(bucket, key)
  end

  defp start_child(bucket) do
    __MODULE__.Supervisor.start_child(:worker, ConCache, [[], [name: bucket]], id: bucket)
  end

  def keys(bucket), do: ConCache.get_all(bucket)
  def put(bucket, key, val), do: ConCache.put(bucket, key, val)
  def add(bucket, key, val), do: ConCache.insert_new(bucket, key, val)
  def get(bucket, key), do: ConCache.get(bucket, key)
  def delete(bucket, key), do: ConCache.delete(bucket, key)
  def update(bucket, key, fun), do: ConCache.update(bucket, key, fun)
  def update_existing(bucket, key, fun), do: ConCache.update_existing(bucket, key, fun)
  def get_or_store(bucket, key, fun), do: ConCache.get_or_store(bucket, key, fun)

  def dirty_put(bucket, key, val), do: ConCache.dirty_put(bucket, key, val)
  def dirty_add(bucket, key, val), do: ConCache.dirty_insert_new(bucket, key, val)
  def dirty_delete(bucket, key), do: ConCache.dirty_delete(bucket, key)
  def dirty_update(bucket, key, fun), do: ConCache.dirty_update(bucket, key, fun)
  def dirty_update_existing(bucket, key, fun),
      do: ConCache.dirty_update_existing(bucket, key, fun)
  def dirty_get_or_store(bucket, key, fun),
      do: ConCache.dirty_get_or_store(bucket, key, fun)
end

defmodule Tellet.Data.Adapters.Cache.Supervisor do
  use Tellet.Supervisor
end
