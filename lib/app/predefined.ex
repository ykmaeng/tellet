defmodule Tellet.App.Predefined do

  import Tellet
  import Logger
  alias Tellet.App
  alias Tellet.XML

  @user_id ""
  @path "priv/apps/"
  @apps [
    "apps",
    "test"
  ]

  def ensure do
    for app <- @apps do
      case App.new(@user_id, app, xml app) do
        :app_already_exists -> :ok
        :ok -> :ok
      end
      debug "App.Predefined.ensure -> #{app}, ok."
    end
  end

  def apps, do: @apps

  def update(app_id) do
    :ok = App.put(@user_id, app_id, XML.trim(xml app_id))
    debug "App.Predefined.update -> #{app_id}, ok."
  end

  def update_all do
    Enum.each @apps, &(update &1)
  end

  def xml(app_id) do
    file = @path <> app_id <> ".xml"
    File.read! file
  end

end
