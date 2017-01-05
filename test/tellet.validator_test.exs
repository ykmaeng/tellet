defmodule Tellet.ValidatorTest do

  use ExUnit.Case, async: true
  use Tellet.Service, as: Service
  use Tellet.Validator, as: V

  setup do
    :ok
  end

  test "available" do
    # rights
    assert V.check(%{"a"=>""}, available: ["a"]) == :ok
    assert V.check(%{"a"=>"", "b"=>""}, available: ["a", "b"]) == :ok
    assert V.check(%{"a"=>"", "b"=>""}, available: ["a", "b", "c"]) == :ok
    # errors
    assert V.check(%{"a"=>"", "b"=>""}, available: ["a"]) == [invalid: "", available: ["a"]]
  end

  test "match" do
    re = ~r/(^(\{\{)*[a-zA-Z]+([\.:_-]*[0-9a-zA-Z]+)*(}})*$)|(^[-0-9]+\.\.[-0-9]{1,5}$)/
    # rights
    assert V.check(%{"a" => "0..0"}, match: {"a", re}) == :ok
    assert V.check(%{"a" => "1..10"}, match: {"a", re}) == :ok
    assert V.check(%{"a" => "-1..01"}, match: re) == :ok
    assert V.check(%{"a" => "9..1"}, match: re) == :ok
    assert V.check(%{"a" => "-9..-1"}, match: re) == :ok

    assert V.check(%{"a" => "r"}, match: re) == :ok
    assert V.check(%{"a" => "req.params"}, match: re) == :ok
    assert V.check(%{"a" => "req.params:array"}, match: re) == :ok

    #assert V.check(%{"a" => "{{a}}"}, match: re) == :ok
    #assert V.check(%{"a" => "{{a.b.c}}"}, match: re) == :ok

    #errors
    assert V.check(%{"a" => "1.."}, match: re) == [invalid: "", match: re]
    assert V.check(%{"a" => "..1"}, match: re) == [invalid: "", match: re]
  end

end


