defmodule Tellet.User do

  @bucket __MODULE__

  defmodule Profile do
    #@derive [Access]
    defstruct id: nil,
              key: nil,
              __secret: nil,
              email: nil,
              country: nil,
              language: nil,
              timezone: nil,
              balance: 0,
              created: nil,
              enabled: false

    @type t :: %Profile{
      id: String.t,
      balance: non_neg_integer,
      created: pos_integer,
      enabled: boolean
    }
  end


  use Timex
  alias Tellet.Data
  import Logger
  
  def profile(id) do
    case Data.get(@bucket, id) do
      {:error, _} -> :user_notfound
      profile -> {:ok, profile}
    end
  end

  def set_active(user, active?) do
    user = %{user | active?: active?}
    Data.put(@bucket, user.id, user)
  end

  def signup(email) do
    :ok
  end

  def new(id, pw, email) do
    profile = %Tellet.User.Profile{
      id: id,
      key: email,
      __secret: Tellet.Util.sha256(pw),
      email: email,
      created: Date.universal |> DateFormat.format!("{RFC1123}"),
      enabled: true
    }
    Data.persist(@bucket, id, profile)
  end

  def auth_basic(user, secret) do
    user.__secret == secret && :ok || :auth_not_matched
  end


  defmodule Test do
    use Timex
    def profile(id) do
      """
      id: nil,
      key: nil,
      spw: nil,
      permits: [],
      active?: false,
      profile: @profile,
      history: @history
      """
      key = "winfavor@gmail.com"
      user = %Tellet.User.Profile{
        id: id,
        key: key,
        __secret: Tellet.Util.sha256(key <> ":" <> "0909"),
        email: "winfavor@gmail.com",
        created: Date.universal |> DateFormat.format!("{RFC1123}"),
        enabled: true
      }
      {:ok, user}
    end
  end


end
