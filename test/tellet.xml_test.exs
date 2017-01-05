defmodule Tellet.XMLTest do

  use ExUnit.Case, async: true
  use Tellet.XML, as: XML

  setup do
    :ok
  end

  test "sax parsing" do
    xml = """
    <tellet>
      <fun _id="foo" name="" age="">
        <return>true</return>
      </fun>
      <set lang="ko-KR"/>
      <block>
        <response>
          <![CDATA[
            Hello World!
          ]]>
        </response>
      </block>
      <stop/>
    </tellet>
    """
    parsed = XML.sax_parse!("myapp", xml)
    assert parsed[:main] ==
      {{Tellet.Services.Core, :tellet, nil}, [
        {{Tellet.Services.Core, :set, %{"lang" => "ko-KR"}}, []},
        {{Tellet.Services.Core, :block, nil}, [
            {{Tellet.Services.Core, :response, %{"val" => "Hello World!"}}, []}
        ]},
        {{Tellet.Services.Core, :stop, nil}, []}
      ]}
    assert parsed["myapp.foo"] ==
      {{Tellet.Services.Core, :fun, %{"_id" => "foo", "name" => "", "age" => ""}}, [
        {{Tellet.Services.Core, :return, %{"val" => "true"}}, []}
      ]}
  end

  test "digraph" do
    g = :digraph.new([:acyclic, :private])
    :digraph.add_vertex(g, 1, :tellet)
    :digraph.add_vertex(g, 2, :set)
    :digraph.add_vertex(g, 3, :block)
    :digraph.add_vertex(g, 4, :stop)
    :digraph.add_edge(g, 1, 2)
    :digraph.add_edge(g, 1, 3)
    :digraph.add_edge(g, 3, 4)
    assert XML.sax_graph_to_node(g, [1]) ==
      [tellet: [set: [], block: [stop: []]]]
  end

end


