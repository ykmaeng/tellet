defmodule Tellet do

  use Application
  use Tellet.Ports.HTTP
  use Tellet.Supervisor
  import Logger

  defmacro __using__(opts) do
    quote do
      import Tellet
    end
  end # defmacro

  defmacro nilstr, do: "\0"

  def start(_type, _args) do
    #start_https Tellet.conf(Tellet.Ports.HTTP, :https)
    start_http Tellet.conf(Tellet.Ports.HTTP, :http)
    sup = start_sup name: Tellet.Supervisor
    Tellet.App.Predefined.ensure
    sup
  end

  def conf(module, key, default \\ nil) do
    Application.get_env(:tellet, module)[key] || default
  end

end
