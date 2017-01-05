defmodule Tellet.Mixfile do
  use Mix.Project

  def project do
    [app: :tellet,
     version: "0.0.1",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.0",
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [
      applications: [
        :logger,
        :dex,
        :httpoison,
        #:con_cache,
        #:amqp,
        #:erlang_js,
        #:riak_core,
      ],
      included_applications: [],
      mod: {Tellet, []}
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # To depend on another app inside the umbrella:
  #
  #   {:myapp, in_umbrella: true}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [
      ## umbrella projects
      {:dex, in_umbrella: true},
      
      ## the others
      {:meck, github: "eproxus/meck", override: true},
      {:exrm, "~> 0.14.12"},
      #{:relx, github: "erlware/relx"},
      {:fsm, "~> 0.2.0"},
      {:plug, "~> 1.0"},
      #{:con_cache, "~> 0.7.0"},
      {:cowboy, "~> 1.0", optional: true},
      {:exactor, "~> 2.1.1", override: true},
      {:httpoison, "~> 0.6"},
      {:timex, "~> 0.19.4"},
      {:amqp, "~> 0.1.1"},
      {:pipe, github: "batate/elixir-pipes"},
      {:erlsom, github: "willemdj/erlsom"},
      #{:erlang_js, github: "basho/erlang_js"},
      #{:riak_core, github: "basho/riak_core"},
      #{:riakc, github: "basho/riak-erlang-client"},
      #{:rivus_cep, github: "vascokk/rivus_cep"},
      {:eiconv, github: "zotonic/eiconv"},
      {:gen_smtp, github: "Vagabond/gen_smtp"},
    ]
  end
end
