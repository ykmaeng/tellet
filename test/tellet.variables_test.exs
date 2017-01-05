defmodule Tellet.VariablesTest do

  use ExUnit.Case, async: true
  use Tellet.Service, as: Service
  use Timex

  setup do
    {:ok, [
      foo: "bar"   
    ]}
  end

  test "test", %{foo: x} do
    x
  end

end


