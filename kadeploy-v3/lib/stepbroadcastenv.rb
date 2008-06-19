require 'lib/debug'

module BroadcastEnvironment
  class BroadcastEnvFactory
    attr_accessor :klass

    def initialize(kind, max_retries, nodes, queue_manager, window_manager, output)
      case kind
      when /BroadcastEnvChainWithFS/
        @klass = BroadcastEnvChainWithFS.new(max_retries, nodes, queue_manager, window_manager, output)
      else
        raise "Invalid kind of step value for the environment broadcast step"
      end
    end
  end

  class BroadcastEnv
    @remaining_retries = 0
    @nodes = nil
    @queue_manager = nil
    @window_manager = nil
    @output = nil

    def initialize(max_retries, nodes, queue_manager, window_manager, output)
      @remaining_retries = max_retries
      @nodes = nodes
      @queue_manager = queue_manager
      @window_manager = window_manager
      @output = output
    end

    def get_macro_step_name
      return self.class.superclass.to_s.split("::")[1]
    end
  end

  class BroadcastEnvChainWithFS < BroadcastEnv
    def run
      Thread.new {
        @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      }   
    end
  end
end
