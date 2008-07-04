require 'lib/debug'

module BroadcastEnvironment
  class BroadcastEnvFactory
    attr_accessor :klass

    def initialize(kind, max_retries, cluster, nodes, queue_manager, window_manager, output)
      case kind
      when "BroadcastEnvChainWithFS"
        @klass = BroadcastEnvChainWithFS.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      when "BroadcastEnvTreeWithFS"
        @klass = BroadcastEnvTreeWithFS.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      else
        raise "Invalid kind of step value for the environment broadcast step"
      end
    end
  end

  class BroadcastEnv
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
      @cluster = cluster
      @step = MicroStepsLibrary::MicroSteps.new(@nodes_ok, @nodes_ko, @window_manager, @queue_manager.config, cluster)
    end

    def get_macro_step_name
      return self.class.superclass.to_s.split("::")[1]
    end
  end

  class BroadcastEnvChainWithFS < BroadcastEnv
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          @nodes_ko.duplicate_and_free(@nodes_ok)
          @output.debugl(3, "Performing a BroadcastEnvChainWithFS step on the nodes: #{@nodes_ok.to_s}")

          #Here are the micro steps 
          @step.send_tarball("chain")
          @step.uncompress_tarball
          @step.copy_kernel_initrd_to_pxe
          @step.switch_pxe("deploy_to_deployed_env")
          #End of micro steps

          @remaining_retries -= 1
          if not @nodes_ok.empty? then
            @nodes_ok.duplicate_and_free(@nodes)
            @queue_manager.next_macro_step(get_macro_step_name, @nodes)
          end
        end
        #After several retries, some nodes may still be in an incorrect state
        if not @nodes_ko.empty? then
          #Maybe some other instances are defined
          if not @queue_manager.replay_macro_step_with_next_instance(get_macro_step_name, @cluster, @nodes_ko)
            @queue_manager.add_to_bad_nodes_set(@nodes_ko)
          end
        else
          @queue_manager.decrement_active_threads
        end    
      }
    end
  end

  class BroadcastEnvTreeWithFS < BroadcastEnv
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          @nodes_ko.duplicate_and_free(@nodes_ok)
          @output.debugl(3, "Performing a BroadcastEnvChainWithFS step on the nodes: #{@nodes_ok.to_s}")

          #Here are the micro steps 
          @step.send_tarball("tree")
          @step.uncompress_tarball
          @step.copy_kernel_initrd_to_pxe
          @step.switch_pxe("deploy_to_deployed_env")
          #End of micro steps

          @remaining_retries -= 1
          if not @nodes_ok.empty? then
            @nodes_ok.duplicate_and_free(@nodes)
            @queue_manager.next_macro_step(get_macro_step_name, @nodes)
          end
        end
        #After several retries, some nodes may still be in an incorrect state
        if not @nodes_ko.empty? then
          #Maybe some other instances are defined
          if not replay_macro_step_with_next_instance(get_macro_step_name, @cluster, @nodes_ko)
            @queue_manager.add_to_bad_nodes_set(@nodes_ko)
          end
        else
          @queue_manager.decrement_active_threads
        end
      }
    end
  end
end