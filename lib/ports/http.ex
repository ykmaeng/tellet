defmodule Tellet.Ports.HTTP do

  use Tellet.Code, as: Code
  use Plug.Builder
  import Plug.Conn
  import Logger

  plug Plug.Parsers, parsers: [:urlencoded]

  defmacro __using__(_opts \\ []) do
    quote do
      defp start_http(opts) do
        debug "Running HTTP with Cowboy on http://localhost:#{opts[:port]}"
        Plug.Adapters.Cowboy.http unquote(__MODULE__), opts, port: opts[:port]
      end
      
      defp start_https(opts) do
        debug "Running HTTPS with Cowboy on https://localhost:#{opts[:port]}"
        Plug.Adapters.Cowboy.https unquote(__MODULE__), opts,
          port: opts[:port],
          otp_app: opts[:app],
          keyfile: opts[:key],
          certfile: opts[:cert] 
      end
    end
  end

  def init(options) do
    #debug "Ports.HTTP init -> options: #{inspect options}"
    options
  end

  def call(conn, opts) do
    #debug "Ports.HTTP call -> conn: #{inspect conn}"
    handle_call conn.path_info, conn, opts
  end

  defp handle_call([], conn, _opts) do
    send_resp(conn, Code.no_content, "")
  end

  defp handle_call(path, conn, opts) do
    {:ok, body, conn} = fetch_query_params(conn) |> read_body
    peer = %Tellet.Request.Peer{
      ip: elem(conn.peer, 0) |> Tuple.to_list,
      port: elem(conn.peer, 1),
      remote: conn.remote_ip |> Tuple.to_list
    }
    req = %Tellet.Request{
      id: Tellet.Util.unique,
      user: String.split(conn.host, ".") |> List.first,
      peer: peer,
      pid: self,
      port: :http,
      app: List.first(path),
      host: conn.host,
      path: full_path(conn),
      route: path,
      body: body,
      params: conn.params,
      method: conn.method,
      #cookies: conn.req_cookies,
      headers: Enum.into(conn.req_headers, %{})
    }
    opts[:callback].request req, fn(res) ->
      conn
        |> delete_resp_header("cache-control")
        |> set_header(res.headers)
        |> send_resp(res.code || (res.body || Code.no_content), to_string res.body || "")
    end
  end

  defp set_header(conn, nil), do: conn
  defp set_header(conn, headers) do
    Enum.reduce headers, conn, fn
      {"content-type" <> _ = k, v}, acc ->
        put_resp_header(acc, k, v <> ";charset=utf-8")
      {k, v}, acc ->
        put_resp_header(acc, k, v)
    end
  end

end

