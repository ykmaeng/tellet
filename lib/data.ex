defmodule Tellet.Data do

  alias Tellet.Data.Adapters.Riak
  alias Tellet.Data.Adapters.Cache

  def ensure_bucket(bucket) do
    Process.whereis(bucket) || Cache.new_bucket(bucket)
  end
    
  def put(bucket, key, val) do
    :ok = memoize(bucket, key, val)
    :ok = persist(bucket, key, val)
  end

  def get(bucket, key) do
    memoized(bucket, key) || persisted(bucket, key)
  end

  def delete(bucket, key) do
    :ok = unmemoize(bucket, key)
    :ok = unpersist(bucket, key)
  end 

  def memoize(bucket, key, val) do
    ensure_bucket bucket
    Cache.dirty_put bucket, key, val
  end

  def memoized(bucket, key) do
    ensure_bucket bucket
    Cache.get(bucket, key)
  end

  def unmemoize(bucket, key) do
    ensure_bucket bucket
    Cache.dirty_delete bucket, key
  end

  def persist(bucket, key, val) do
    Riak.put to_binary(bucket), to_binary(key), val
  end

  def persisted(bucket, key) do
    case Riak.get to_binary(bucket), to_binary(key) do
      {:ok, val} when is_binary(val) -> val
      {:ok, val} -> :erlang.binary_to_term val
      error -> error
    end
  end

  def unpersist(bucket, key) do
    Riak.delete to_binary(bucket), to_binary(key)
  end

  defp to_binary(data) when is_atom(data), do: Atom.to_string(data)
  defp to_binary(data) when not is_binary(data), do: :erlang.term_to_binary data
  defp to_binary(data), do: data
end
