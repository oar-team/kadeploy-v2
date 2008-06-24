require 'lib/debug'
require 'lib/microsteps'
require 'lib/nodes'

module SetDeploymentEnvironnment

  class SetDeploymentEnvFactory
    attr_accessor :klass

    def initialize(kind, max_retries, cluster, nodes, queue_manager, window_manager, output)
      case kind
      when /SetDeploymentEnvUntrusted/
        @klass = SetDeploymentEnvUntrusted.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      when /SetDeploymentEnvNfsroot/
        @klass =  SetDeploymentEnvNfsroot.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      when /SetDeploymentEnvProd/
        @klass =  SetDeploymentEnvProd.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      when /SetDeploymentEnvDummy/
        @klass =  SetDeploymentEnvDummy.new(max_retries, cluster, nodes, queue_manager, window_manager, output)
      else
        raise "Invalid kind of step value for the environment deployment step"
      end
    end
  end

  class SetDeploymentEnv
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

  class SetDeploymentEnvUntrusted < SetDeploymentEnv
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          @nodes_ko.duplicate_and_free(@nodes_ok)
          @output.debugl(3, "Performing a SetDeploymentEnvUntrusted step on the nodes: #{@nodes_ok.to_s}")
          @step.switch_pxe("prod_to_deploy_env")
          @step.reboot("soft")
          @step.check_nodes("deploy_env_booted")
          @step.load_drivers
          @step.do_fdisk
          @step.do_format
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

  class SetDeploymentEnvProd < SetDeploymentEnv
    def run
      Thread.new {
        @queue_manager.increment_active_threads
        @nodes.duplicate_and_free(@nodes_ko)
        while (@remaining_retries > 0) && (not @nodes_ko.empty?)
          @nodes_ko.duplicate_and_free(@nodes_ok)
          @output.debugl(3, "Performing a SetDeploymentEnvProd step on the nodes: #{@nodes_ok.to_s}")

          #Here are the micro steps 
          @step.check_nodes("prod_env_booted")
          @step.fdisk
          @step.format_deploy_part
          @step.mount_deploy_part
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

  
  class SetDeploymentEnvNfsroot < SetDeploymentEnv
    def run
      @queue_manager.increment_active_threads
      @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      @queue_manager.decrement_active_threads
    end
  end

  class SetDeploymentEnvDummy < SetDeploymentEnv
    def run
      @queue_manager.increment_active_threads
      @queue_manager.next_macro_step(get_macro_step_name, @nodes)
      @queue_manager.decrement_active_threads
    end
  end
end
