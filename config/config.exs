# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for third-
# party users, it should be done in your mix.exs file.

# Sample configuration:
#
#     config :logger, :console,
#       level: :info,
#       format: "$date $time [$level] $metadata$message\n",
#       metadata: [:user_id]

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env}.exs"


# sasl
config :sasl, :sasl_error_logger,           {:file, 'log/sasl-error.log'}
config :sasl, :errlog_type,                 :error
config :sasl, :error_logger_mf_dir,         'log/sasl'
config :sasl, :error_logger_mf_maxbytes,    10485760
config :sasl, :error_logger_mf_maxfiles,    5


# lager
config :lager, :handlers,                   lager_console_backend: :info,
                                            lager_file_backend: [
                                              file: 'log/error.log', level: :error
                                            ],
                                            lager_file_backend: [
                                              file: 'log/console.log', level: :info
                                            ]
config :lager, :crash_log,                  'log/crash.log'
config :lager, :crash_log_msg_size,         65536
config :lager, :crash_log_size,             10485760
config :lager, :crash_log_date,             '$D0'
config :lager, :crash_log_count,            5
config :lager, :error_logger_redirect,      true


# riak_core
config :riak_core, :platform_data_dir,      'priv/riak_core/data'
config :riak_core, :platform_bin_dir,       'priv/riak_core/bin'
config :riak_core, :platform_etc_dir,       'priv/riak_core/etc'
config :riak_core, :platform_lib_dir,       'priv/riak_core/lib'
config :riak_core, :platform_log_dir,       'priv/riak_core/log'
config :riak_core, :handoff_port,           61001
config :riak_core, :ring_creation_size,     8
config :riak_core, :default_bucket_props,   n_val: 3
config :riak_core, :handoff_concurrency,    2
config :riak_core, :ring_state_dir,         'priv/riak_core/data/ring'
config :riak_core, :ssl,                    certfile: 'priv/riak_core/etc/cert.pem',
                                            keyfile: 'priv/riak_core/etc/key.pem',
                                            cacertfile: 'priv/riak_core/etc/cacertfile.pem'
config :riak_core, :enable_consensus,       false
config :riak_core, :use_background_manager, false
config :riak_core, :vnode_management_timer, 10_000


# pooler
config :pooler, :pools, [
  [
    name: Dex.JS,
    group: :dex_js,
    max_count: 10,
    init_count: 10,
    start_mfa: {Dex.JS, :start_link, []}
  ]
]

# dex
config Dex, Dex.JS, 
  libs: [
    "jstat.min.js"
  ]


# tellet
config :tellet, Tellet, nil

config :tellet, Tellet.Ports.HTTP,
  https: [
    key: "ssl/tellet.io.key",
    cert: "ssl/unified.crt",
    app: :tellet,
    port: 8000,
    callback: Tellet.Ports.HTTP.Handler
  ],
  http: [
    port: 8080,
    callback: Tellet.Ports.HTTP.Handler
  ]

config :tellet, Tellet.Supervisor,
  children: [
    supervisor: Tellet.Data.Supervisor,
    #worker: Tellet.Services.Message.Rabbit
  ]

config :tellet, Tellet.Data.Supervisor,
  children: [
    #worker: Tellet.Data.Adapters.Riak,
    #worker: Tellet.Data.Adapters.Cache,
    #supervisor: Tellet.Data.Adapters.Cache.Supervisor
  ]

config :tellet, Tellet.Data.Adapters.Riak,
  args: [
    [host: '127.0.0.1', port: 8087]
  ]

config :tellet, Tellet.Data.Adapters.Cache,
  buckets: [
    :test
  ]

config :tellet, Tellet.Data.Adapters.Cache.Supervisor, nil

config :tellet, Tellet.App.Resource,
  bucket: Tellet.App.Resource

config :tellet, Tellet.Services.Message.Rabbit,
  vhost: "tellet",
  args: [
    [addr: "amqp://guest:guest@127.0.0.1/tellet"]
  ]

