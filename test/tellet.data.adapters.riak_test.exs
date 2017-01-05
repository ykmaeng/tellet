defmodule Tellet.Data.Adapters.RiakTest do

  use ExUnit.Case, async: true
  alias Tellet.Data.Adapters.Riak

  setup do
    #Riak.start_link
    :ok
  end

  test "input & output" do
    assert Riak.echo(:hello) == :hello
    assert Riak.delete("test", "new") == :ok
    assert Riak.get("test", "new") == {:error, :notfound}
    assert Riak.put("test", "new", "value") == :ok
    assert Riak.get("test", "new") == {:ok, "value"}
  end
end

