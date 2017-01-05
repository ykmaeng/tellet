defmodule Tellet.Response do

  use Tellet.Code

  @type code :: pos_integer
  @type body :: bitstring
  @type type :: bitstring

  defstruct code: nil,
            headers: nil,
            body: nil

  defmacro __using__(opts \\ []) do
    quote do
      alias unquote(__MODULE__), unquote(opts)
    end
  end # defmacro

end
