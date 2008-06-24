require 'lib/debug'

module BootNewEnvironment
  class BootNewEnvFactory
    attr_accessor :klass

    def initialize(kind, max_retries, cluster, nodes, queue_manager, window_manager, output)
      case kind
      when /BootNewEnvKexec/
        @klass = BootNewEnvKexec.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      when /BootNewEnvClassical/
        @klass = BootNewEnvClassical.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      else
        raise "Invalid kind of step value for the new environment boot step"
      end
    end
  end

  class BootNewEnv
    @remaining_retries = 0
    @nodes = nil
    @queue_manager = nil
    @window_manager = nil
    @output = nil

    def initialize(max_retries, cluster, nodes, queue_manager, window_manager, output)
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

  class BootNewEnvKexec < BootNewEnv
    def run
      @queue_manager.increment_active_threads
      Thread.new {
        @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      }
      @queue_manager.decrement_active_threads
    end
  end

  class BroadcastEnvClassical < BootNewEnv
    def run
      @queue_manager.increment_active_threads
      Thread.new {
        @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      }
      @queue_manager.decrement_active_threads
    end
  end
end
