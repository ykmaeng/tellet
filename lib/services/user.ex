defmodule Tellet.Services.User do

  defmacro __using__(opts) do
    quote do
      import unquote(__MODULE__)
    end
  end # defmacro

end
