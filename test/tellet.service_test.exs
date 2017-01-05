defmodule Tellet.ServiceTest do

  use ExUnit.Case, async: true
  use Tellet.Service, as: Service
  use Tellet.XML, as: XML

  setup do
    :ok
  end

  test "fun node" do
    xml = """
    <tellet>
      <fun:foo>
        <return>foo</return>
      </fun:foo>
      <fun _id="bar">
        <return>bar</return>
      </fun>
    </tellet>
    """
    parsed = XML.sax_parse!("myapp", xml)
    assert parsed["myapp.foo"] ==
      {{Tellet.Services.Core, :fun, %{"_id" => "foo"}}, [
        {{Tellet.Services.Core, :return, %{"val" => "foo"}}, []}
      ]}
  end
end

