defmodule Tellet.Services.App do

  use Tellet.Code, as: Code
  use Tellet.XML, as: XML
  use Tellet.Variables, as: Vars
  use Tellet.Service
  alias Tellet.App
  import Logger

  defunp new args, state do
    user_id = state.req.user.id
    app_id = args["id"] || ""
    body = args["body"]
    userdata = args["userdata"] || ""
    case App.new(user_id, app_id, body, userdata) do
      :ok -> set_code(state, Code.created)
      :app_already_exists -> set_code(state, Code.conflict)
    end
    next! state
  end

  defunp get args, state do
    app_id = args["id"] || ""
    case App.get(state.req.user.id, app_id) do
      {:ok, app} ->
        app = Map.from_struct(app)
        Vars.to_var(state.data, app, "val")
      _error ->
        set_code(state, Code.not_found)
    end
    next! state
  end

  defunp put args, state do
    user_id = state.req.user.id
    app_id = args["id"] || ""
    body = String.lstrip args["body"]
    userdata = args["userdata"] || ""
    IO.inspect args: args
    case App.put(user_id, app_id, body, userdata) do
      :ok -> set_code(state, Code.ok)
      {:error, reason} -> set_codebody(state, Code.conflict, reason)
    end
    next! state
  end

  defunp del args, state do
    user_id = state.req.user.id
    app_id = args["id"] || ""
    case App.del(user_id, app_id) do
      :ok -> set_code(state, Code.ok)
      {:error, reason} -> set_codebody(state, Code.not_found, reason)
    end
    next! state
  end

end
