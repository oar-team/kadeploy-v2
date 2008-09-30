require 'lib/debug'

module BroadcastEnvironment
  class BroadcastEnvFactory
    # Factory for the methods to broadcast an environment
    #
    # Arguments
    # * kind: specifies the method to use (BroadcastEnvChainWithFS, BroadcastEnvTreeWithFS, BroadcastEnvDummy)
    # * max_retries: maximum number of retries for the step
    # * timeout: timeout for the step
    # * cluster: name of the cluster
    # * nodes: instance of NodeSet
    # * queue_manager: instance of QueueManager
    # * reboot_window: instance of WindowManager
    # * output: instance of OutputControl
    # * logger: instance of Logger
    # Output
    # * returns a BroadcastEnv instance (BroadcastEnvChainWithFS, BroadcastEnvTreeWithFS, BroadcastEnvDummy)
    # * raises an exception if an invalid kind of instance is given
    def BroadcastEnvFactory.create(kind, max_retries, timeout, cluster, nodes, queue_manager, reboot_window, output, logger)
      case kind
      when "BroadcastEnvChainWithFS"
        return BroadcastEnvChainWithFS.new(max_retries, timeout, cluster, nodes, queue_manager, reboot_window, output, logger)
      when "BroadcastEnvTreeWithFS"
        return BroadcastEnvTreeWithFS.new(max_retries, timeout, cluster, nodes, queue_manager, reboot_window, output, logger)
      when "BroadcastEnvDummy"
        return BroadcastEnvDummy.new(max_retries, timeout, cluster, nodes, queue_manager, reboot_window, output, logger)
      else
        raise "Invalid kind of step value for the environment broadcast step"
      end
    end
  end

  class BroadcastEnv
    @remaining_retries = 0
    @timeout = 0
    @queue_manager = nil
    @config = nil
    @reboot_window = nil
    @output = nil
    @cluster = nil
    @nodes = nil
    @nodes_ok = nil
    @nodes_ko = nil
    @step = nil
    @start = nil

    # Constructor of BroadcastEnv
    #
    # Arguments
    # * max_retries: maximum number of retries for the step
    # * timeout: timeout for the step
    # * cluster: name of the cluster
    # * nodes: instance of NodeSet
    # * queue_manager: instance of QueueManager
    # * reboot_window: instance of WindowManager
    # * output: instance of OutputControl
    # * logger: instance of Logger
    # Output
    # * nothing
    def initialize(max_retries, timeout, cluster, nodes, queue_manager, reboot_window, output, logger)
      @remaining_retries = max_retries
      @timeout = timeout
      @nodes = nodes
      @queue_manager = queue_manager
      @config = @queue_manager.config
      @reboot_window = reboot_window
      @output = output
      @nodes_ok = Nodes::NodeSet.new
      @nodes_ko = Nodes::NodeSet.new
      @cluster = cluster
      @logger = logger
      @logger.set("step2", get_instance_name, @nodes)
      @logger.set("timeout_step2", @timeout, @nodes)
      @start = Time.now.to_i
      @step = MicroStepsLibrary::MicroSteps.new(@nodes_ok, @nodes_ko, @reboot_window, @config, cluster, output)
    end

    # Get the name of the current macro step
    #
    # Arguments
    # * nothing
    # Output
    # * returns the name of the current macro step  
    def get_macro_step_name
      return self.class.superclass.to_s.split("::")[1]
    end

    # Get the name of the current instance
    #
    # Arguments
    # * nothing
    # Output
    # * returns the name of the current current instance
    def get_instance_name
      return self.class.to_s.split("::")[1]
    end
  end

  class BroadcastEnvChainWithFS < BroadcastEnv
    # Main of the BroadcastEnvChainWithFS instance
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          instance_thread = Thread.new {
            @logger.increment("retry_step2", @nodes_ko)
            @nodes_ko.duplicate_and_free(@nodes_ok)
            @output.debugl(3, "Performing a BroadcastEnvChainWithFS step on the nodes: #{@nodes_ok.to_s}")
            result = true
            #Here are the micro steps
            result = result && @step.send_tarball_and_uncompress("chain")
            result = result && @step.send_key("chain")
            result = result && @step.copy_kernel_initrd_to_pxe
            result = result && @step.switch_pxe("deploy_to_deployed_env")
            #End of micro steps
          }
          if not @step.timeout?(@timeout, instance_thread, get_macro_step_name) then
            if not @nodes_ok.empty? then
              @logger.set("step2_duration", Time.now.to_i - @start, @nodes_ok)
              @nodes_ok.duplicate_and_free(@nodes)
              @queue_manager.next_macro_step(get_macro_step_name, @nodes)
            end
          end
          @remaining_retries -= 1
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
    # Main of the BroadcastEnvTreeWithFS instance
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          instance_thread = Thread.new {
            @logger.increment("retry_step2", @nodes_ko)
            @nodes_ko.duplicate_and_free(@nodes_ok)
            @output.debugl(3, "Performing a BroadcastEnvChainWithFS step on the nodes: #{@nodes_ok.to_s}")
            result = true
            #Here are the micro steps 
            result = result && @step.send_tarball_and_uncompress("tree")
            result = result && @step.send_key("tree")
            result = result && @step.copy_kernel_initrd_to_pxe
            result = result && @step.switch_pxe("deploy_to_deployed_env")
            #End of micro steps
          }
          if not @step.timeout?(@timeout, instance_thread, get_macro_step_name) then
            if not @nodes_ok.empty? then
              @logger.set("step2_duration", Time.now.to_i - @start, @nodes_ok)
              @nodes_ok.duplicate_and_free(@nodes)
              @queue_manager.next_macro_step(get_macro_step_name, @nodes)
            end
          end
          @remaining_retries -= 1
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

  class BroadcastEnvDummy < BroadcastEnv
    # Main of the BroadcastEnvDummy instance
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def run
      @config.common.taktuk_connector = @config.common.taktuk_ssh_connector
      @queue_manager.increment_active_threads
      @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      @queue_manager.decrement_active_threads
    end
  end
end
