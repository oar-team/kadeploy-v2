require 'lib/debug'
require 'lib/microsteps'
require 'lib/nodes'

module SetDeploymentEnvironnment

  class SetDeploymentEnvFactory
    attr_accessor :klass

    def initialize(kind, max_retries, nodes, queue_manager, window_manager, output)
      case kind
      when /SetDeploymentEnvUntrusted/
        @klass = SetDeploymentEnvUntrusted.new(max_retries, nodes, queue_manager, window_manager, output)
      when /SetDeploymentEnvNfsroot/
        @klass =  SetDeploymentEnvNfsroot.new(max_retries, nodes, queue_manager, window_manager, output)
      when /SetDeploymentEnvProd/
        @klass =  SetDeploymentEnvProd.new(max_retries, nodes, queue_manager, window_manager, output)
      when /SetDeploymentEnvDummy/
        @klass =  SetDeploymentEnvDummy.new(max_retries, nodes, queue_manager, window_manager, output)
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
    @nodes = nil
    @nodes_ok = nil
    @nodes_ko = nil
    @step = nil

    def initialize(max_retries, nodes, queue_manager, window_manager, output)
      @remaining_retries = max_retries
      @nodes = nodes
      @queue_manager = queue_manager
      @window_manager = window_manager
      @output = output
      @nodes_ok = Nodes::NodeSet.new
      @nodes_ko = Nodes::NodeSet.new
      @step = MicroStepsLibrary::MicroSteps.new(@nodes, @nodes_ok, @nodes_ko, @window_manager)
    end

    def get_macro_step_name
      return self.class.superclass.to_s.split("::")[1]
    end
  end

  class SetDeploymentEnvUntrusted < SetDeploymentEnv
    def run
      Thread.new {
        while (@remaining_retries > 0) && ((@nodes_ok.empty? && @nodes_ko.empty?) || (not @nodes_ko.empty?))
          @output.debugl(3, "Performing a SetDeploymentEnvUntrusted step on the nodes: #{@nodes.to_s}")
          @step.switch_pxe("prod_to_deploy_env")
          @step.reboot("soft")
          @step.check_nodes("deploy_env_booted")
          @step.load_drivers
          @step.do_fdisk
          @step.do_format
          @remaining_retries -= 1
          @queue_manager.next_macro_step(get_macro_step_name, @nodes)   
        end
      }
    end
  end
  
  class SetDeploymentEnvNfsroot < SetDeploymentEnv

  end

  class SetDeploymentEnvProd < SetDeploymentEnv

  end

  class SetDeploymentEnvDummy < SetDeploymentEnv

  end
end
