defmodule Tellet.Ports.HTTP.Handler do
 
  use Pipe
  use Tellet.FSM, state: nil, data: nil
  use Tellet.Request, as: Request
  use Tellet.Code, as: Code

  alias Tellet.Worker
  alias Tellet.User
  alias Tellet.App
  alias Tellet.Util

  import Logger

  def request(req, res) do
    #debug "Request.Handlers.HTTP request -> #{inspect req}"
    try do
      fsm = pipe_while fn(x) ->
        x.state && debug "Request.Handlers.HTTP request -> #{x.state}"; true
      end, new
      |> check_user(req)
     #|> authenticate
      |> check_app
      |> begin 
      |> response
      debug "Request.Handlers.HTTP request -> #{fsm.state}"
      res.(fsm.data)
    rescue
      exception ->
        #msg = Exception.message(exception)
        error inspect error: inspect(exception), trace: System.stacktrace
        %Tellet.Response{code: Code.service_unavilable} |> res.()
    catch
      :throw, error ->
        handle_throw(error)
        %Tellet.Response{code: error_to_code error} |> res.()
    end
  end

  defp handle_throw(error) do
    warn "Request.Handlers.HTTP request -> throw, #{error}"
  end

  defp error_to_code(:error), do: Code.bad_request
  defp error_to_code(:badfun), do: Code.bad_request
  defp error_to_code(:forbidden), do: Code.forbidden
  defp error_to_code(:unauthorized), do: Code.unauthorized
  defp error_to_code(:app_notfound), do: Code.not_found
  defp error_to_code(_), do: Code.bad_request

  defp log(%{state: state}) do
    state && debug "Request.Handlers.HTTP request -> #{state}"; true
  end

  defevent check_user(req) do
    case User.Test.profile(req.user) do
      {:ok, user} ->
        unless user.enabled do
          next_state(:disabled_user)
        else
          next_state :valid_user, %{req | user: user}
        end
      state ->
        next_state state
    end
  end

  defstate valid_user do
    defevent authenticate, data: req do
      header_auth = req.headers["authorization"]
      case header_auth do
        "Basic " <> base64 ->
          case Base.decode64(base64) do
              {:ok, decoded} ->
                secret = Util.sha256(decoded)
                case User.auth_basic(req.user, secret) do
                  :ok ->
                    req = %{req | __authorized: true}
                    next_state :authorized, req
                  state ->
                    next_state state
                end
              :error ->
                next_state :auth_wrong_format
          end
        _ ->
          next_state :auth_ignored, req
      end
    end

    defevent check_app, data: req do
      handle_event(:check_app, req)
    end
  end

  defstate auth_ignored do
    defevent check_app, data: req, do: handle_event(:check_app, req)
  end

  defstate authorized do
    defevent check_app, data: req, do: handle_event(:check_app, req)
  end

  defstate valid_app do
    defevent begin, data: {req, app} do
      next_state(:working, %{req | app: app})
    end
  end

  defstate working do
    defevent response, data: req do
      res = Worker.begin req
      next_state :finished, res
    end
  end

  defevent _, state: state do
    throw state
  end

  defp handle_event(:check_app, req) do
    case App.get(req.user.id, req.app) do
      {:ok, app} ->
        unless app.enabled do
          next_state(:disabled_app)
        else
          next_state :valid_app, {req, app}
        end
      state ->
        next_state state
    end
  end

  defp timeout(req) do
    try do
      timeout = req.params["_timeout"]
      timeout && String.to_integer(timeout) || Request.default_timeout
    rescue ArgumentError ->
      Request.default_timeout
    end
  end
end
