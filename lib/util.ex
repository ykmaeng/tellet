defmodule Tellet.Util do

  use Timex

  def unique do
    List.to_string(
      :base62.encode62(<<trunc(Time.now :usecs)::64>>) ++
      :base62.encode62(<<:erlang.phash2(node)::32, :crypto.rand_bytes(4)::binary>>)
    )
  end

  def sha256(str) do
    :crypto.hash(:sha256, str)
  end

  def sha512(str) do
    :crypto.hash(:sha512, str)
  end

end
