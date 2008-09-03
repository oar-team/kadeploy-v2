require 'lib/debug'
require 'lib/microsteps'
require 'lib/nodes'

module SetDeploymentEnvironnment

  class SetDeploymentEnvFactory
    def SetDeploymentEnvFactory.create(kind, max_retries, timeout, cluster, nodes, queue_manager, window_manager, output)
      case kind
      when "SetDeploymentEnvUntrusted"
        return SetDeploymentEnvUntrusted.new(max_retries, timeout, cluster, nodes, queue_manager, window_manager, output)
      when "SetDeploymentEnvNfsroot"
        return SetDeploymentEnvNfsroot.new(max_retries, timeout, cluster, nodes, queue_manager, window_manager, output)
      when "SetDeploymentEnvProd"
        return SetDeploymentEnvProd.new(max_retries, timeout, cluster, nodes, queue_manager, window_manager, output)
      when "SetDeploymentEnvDummy"
        return SetDeploymentEnvDummy.new(max_retries, timeout, cluster, nodes, queue_manager, window_manager, output)
      else
        raise "Invalid kind of step value for the environment deployment step"
      end
    end
  end

  class SetDeploymentEnv
    @remaining_retries = 0
    @timeout = 0
    @queue_manager = nil
    @window_manager = nil
    @output = nil
    @cluster = nil
    @nodes = nil
    @nodes_ok = nil
    @nodes_ko = nil
    @step = nil
    @config = nil

    def initialize(max_retries, timeout, cluster, nodes, queue_manager, window_manager, output)
      @remaining_retries = max_retries
      @timeout = timeout
      @nodes = nodes
      @queue_manager = queue_manager
      @config = @queue_manager.config
      @window_manager = window_manager
      @output = output
      @nodes_ok = Nodes::NodeSet.new
      @nodes_ko = Nodes::NodeSet.new
      @cluster = cluster
      @step = MicroStepsLibrary::MicroSteps.new(@nodes_ok, @nodes_ko, @window_manager, @config, cluster, output)
    end

    def get_macro_step_name
      return self.class.superclass.to_s.split("::")[1]
    end
  end

  class SetDeploymentEnvUntrusted < SetDeploymentEnv
    def run
      @config.common.taktuk_connector = @config.common.taktuk_rsh_connector
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          instance_thread = Thread.new {
            @nodes_ko.duplicate_and_free(@nodes_ok)
            @output.debugl(3, "Performing a SetDeploymentEnvUntrusted step on the nodes: #{@nodes_ok.to_s}")
            result = true
            #Here are the micro steps
            result = result && @step.switch_pxe("prod_to_deploy_env")
            result = result && @step.reboot("soft")
            result = result && @step.wait_reboot(@config.common.rsh_port)
            result = result && @step.fdisk
            result = result && @step.format_deploy_part
            result = result && @step.mount_deploy_part          
            result = result && @step.format_tmp_part
            result = result && @step.mount_tmp_part
            #End of micro steps
          }
          if not @step.timeout?(@timeout, instance_thread, get_macro_step_name) then
            if not @nodes_ok.empty? then
              @nodes_ok.duplicate_and_free(@nodes)
              @queue_manager.next_macro_step(get_macro_step_name, @nodes)
            end
          end
          @remaining_retries -= 1
        end
        #After several retries, some nodes may still be in an incorrect state
        if not @nodes_ko.empty? then
          #Maybe some other instances are defined
          if not @queue_manager.replay_macro_step_with_next_instance(get_macro_step_name, @cluster, @nodes_ko) then
            @queue_manager.add_to_bad_nodes_set(@nodes_ko)
          end
        else
          @queue_manager.decrement_active_threads
        end
      }
    end
  end

  class SetDeploymentEnvProd < SetDeploymentEnv
    def run
      @config.common.taktuk_connector = @config.common.taktuk_ssh_connector
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          instance_thread = Thread.new {
            @nodes_ko.duplicate_and_free(@nodes_ok)
            @output.debugl(3, "Performing a SetDeploymentEnvProd step on the nodes: #{@nodes_ok.to_s}")
            result = true
            #Here are the micro steps
            result = result && @step.check_nodes("prod_env_booted")
            result = result && @step.fdisk
            result = result && @step.format_deploy_part
            result = result && @step.mount_deploy_part
            result = result && @step.format_tmp_part
            result = result && @step.mount_tmp_part
            #End of micro steps
          }
          if not @step.timeout?(@timeout, instance_thread, get_macro_step_name) then
            if not @nodes_ok.empty? then
              @nodes_ok.duplicate_and_free(@nodes)
              @queue_manager.next_macro_step(get_macro_step_name, @nodes)
            end            
          end
          @remaining_retries -= 1
        end
        #After several retries, some nodes may still be in an incorrect state
        if not @nodes_ko.empty? then
          #Maybe some other instances are defined
          if not @queue_manager.replay_macro_step_with_next_instance(get_macro_step_name, @cluster, @nodes_ko) then
            @queue_manager.add_to_bad_nodes_set(@nodes_ko)
          end
        else
          @queue_manager.decrement_active_threads
        end    
      }
    end
  end

  
  class SetDeploymentEnvNfsroot < SetDeploymentEnv
    def run
      @config.common.taktuk_connector = @config.common.taktuk_rsh_connector
      @queue_manager.increment_active_threads
      @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      @queue_manager.decrement_active_threads
    end
  end

  class SetDeploymentEnvDummy < SetDeploymentEnv
    def run
      @config.common.taktuk_connector = @config.common.taktuk_ssh_connector
      @queue_manager.increment_active_threads
      @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      @queue_manager.decrement_active_threads
    end
  end
end
