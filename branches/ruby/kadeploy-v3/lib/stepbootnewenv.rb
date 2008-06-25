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
    @queue_manager = nil
    @window_manager = nil
    @output = nil
    @cluster = nil
    @nodes = nil
    @nodes_ok = nil
    @nodes_ko = nil
    @step = nil
    
    def initialize(max_retries, cluster, nodes, queue_manager, window_manager, output)
      @remaining_retries = max_retries
      @nodes = nodes
      @queue_manager = queue_manager
      @window_manager = window_manager
      @output = output
      @nodes_ok = Nodes::NodeSet.new
      @nodes_ko = Nodes::NodeSet.new
      @step = MicroStepsLibrary::MicroSteps.new(@nodes_ok, @nodes_ko, @window_manager, @queue_manager.config, cluster)
    end

    def get_macro_step_name
      return self.class.superclass.to_s.split("::")[1]
    end
  end

  class BootNewEnvKexec < BootNewEnv
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          @nodes_ko.duplicate_and_free(@nodes_ok)
          @output.debugl(3, "Performing a BootNewEnvKexec step on the nodes: #{@nodes_ok.to_s}")

          #Here are the micro steps 
          @step.reboot("kexec")
          @step.wait_reboot
          #End of micro steps

          @remaining_retries -= 1
          if not @nodes_ok.empty? then
            @nodes_ok.duplicate_and_free(@nodes)
            @queue_manager.next_macro_step(get_macro_step_name, @nodes)
          end
        end
        #After several retries, some nodes may still be in an incorrect state
        if not @nodes_ko.empty? then
          @queue_manager.add_to_bad_nodes_set(@nodes_ko)
        end
        @queue_manager.decrement_active_threads
      }
    end
  end

  class BroadcastEnvClassical < BootNewEnv
    def run
     Thread.new {
        @queue_manager.increment_active_threads
        @queue_manager.next_macro_step(get_macro_step_name, @nodes)
        @queue_manager.decrement_active_threads
      }
    end
  end
end
