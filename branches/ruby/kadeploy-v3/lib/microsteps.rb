require 'lib/debug'
require 'lib/nodes'

module MicroStepsLibrary
  class MicroSteps
    @nodes_ok = nil
    @nodes_ko = nil
    @window_manager = nil

    def initialize(nodes, nodes_ok, nodes_ko, window_manager)
      @nodes_ok = nodes_ok
      @nodes_ko = nodes_ko
      #at the begining we copy the initial set of nodes in the nodes_ko set
      nodes.duplicate(nodes_ko)
      @window_manager = window_manager
    end

    def switch_pxe(kind)
      node_set = Nodes::NodeSet.new
      @nodes_ko.duplicate_and_free(node_set)
      case kind
      when /prod_to_deploy_env/
      when /deploy_to_deployed_env/
      end
      node_set.duplicate_and_free(@nodes_ok)
    end
    
    def reboot(kind)
      case kind
      when /soft/
      when /hard/
      when /veryhard/
      end
    end

    def check_nodes(kind)
      case kind
      when /deploy_env_booted/
      when /deployed_env_booted/
      when /prod_env_booted/
      end
    end

    def load_drivers
    end

    def do_fdisk
    end

    def do_format

    end
  end
end
