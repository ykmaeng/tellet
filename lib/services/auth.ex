defmodule Tellet.Services.Auth do

  use Tellet.Service
  use Tellet.Code, as: Code
  alias Tellet.User
  alias Tellet.Util

  @bucket __MODULE__

  defun basic args, state,
    available: ["realm"]
  do
    req = state.req
    if req.__authorized === true do
      next! state
    else
      fn_throw = fn -> [
        code: Code.unauthorized,
        headers: [
          {"WWW-Authenticate", "Basic realm='"<>(args["realm"] || "")<>"'"}
        ]
      ] end
      case req.headers["authorization"] do
        "Basic " <> base64 ->
          case Base.decode64(base64) do
            {:ok, decoded} ->
              secret = Util.sha256(decoded)
              case User.auth_basic(req.user, secret) do
                :ok ->
                  next! put_in(state.req.__authorized, true)
                _ ->
                  throw fn_throw.()
              end
            :error ->
              throw fn_throw.()
          end
        _ ->
          throw fn_throw.()
      end # case
    end # if
  end 
end


