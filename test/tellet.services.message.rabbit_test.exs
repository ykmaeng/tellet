defmodule Tellet.Services.Message.RabbitTest do

  use ExUnit.Case, async: true
  use Tellet.Service, as: Service
  use Timex
  alias Tellet.Services.Message.Rabbit, as: Rabbit

  setup do
    {:ok, [
      foo: "bar"   
    ]}
  end

  test "declare exchange", %{foo: x} do
    x
  end

end

