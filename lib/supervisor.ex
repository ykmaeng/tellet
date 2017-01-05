defmodule Tellet.Supervisor do

  defmacro __using__(opts) do
    quote do
      #use Supervisor
      
      defp start_sup(args \\ []), do: start_link(args ++ unquote opts)

      def start_link(opts \\ []) do
        Supervisor.start_link(__MODULE__, args=opts, opts)
      end

      def start_child(child) do
        Supervisor.start_child(__MODULE__, child)
      end

      def start_child(type, module, args \\ [], opts \\ []) do
        start_child apply(Supervisor.Spec, type, [module, args, opts])
      end

      def init(opts) do
        sup_name = opts[:name] || __MODULE__
        children = Application.get_env(:tellet, sup_name)[:children] || []
        children = for {type, module} <- children do
          args = Tellet.conf(module, :args) || [[name: module]]
          apply Supervisor.Spec, type, [module, args]
        end
        Supervisor.Spec.supervise(children, strategy: opts[:strategy] || :one_for_one)
      end
    end # qutoe
  end # defmacro

end
