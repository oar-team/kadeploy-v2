require 'contrib/taktuk_wrapper'
require 'yaml'

module ParallelOperations
  class ParallelOps
    @nodes = nil
    @taktuk_connector = nil
    @taktuk_tree_arity = nil

    def initialize(nodes, config)
      @nodes = nodes
      @taktuk_connector = config.common.taktuk_connector
      @taktuk_tree_arity = config.common.taktuk_tree_arity
    end

    def make_taktuk_exec_cmd(cmd)
      args = String.new
      args += " -c #{@taktuk_connector}" if @taktuk_connector != ""
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast exec [ #{cmd} ]"
      return args.split(" ")
    end

    def make_taktuk_send_file_cmd(file, dest_dir)
      args = String.new
      args += " -c #{@taktuk_connector}" if @taktuk_connector != ""
      args += " -d #{@taktuk_tree_arity}"
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast put [ #{file} ] [ #{dest_dir} ]"
      return args.split(" ")
    end

    def init_nodes_state_before_exec_command
      @nodes.set.each { |node|
        node.last_cmd_exit_status = "256"
        node.last_cmd_stderr = "The node #{node.hostname} is unreachable"
      }
    end

    def get_taktuk_exec_command_infos(tw)
      tree = YAML.load((YAML.dump({"hosts"=>tw.hosts,
                                    "connectors"=>tw.connectors,
                                    "errors"=>tw.errors,
                                    "infos"=>tw.infos})))
      init_nodes_state_before_exec_command
      tree['hosts'].each_value { |h|
        h['commands'].each_value { |x|
          @nodes.get_node_by_host(h['host_name']).last_cmd_exit_status = x['status']
          @nodes.get_node_by_host(h['host_name']).last_cmd_stdout = x['output']
          @nodes.get_node_by_host(h['host_name']).last_cmd_stderr = x['error']
        }
      }
    end

    def init_nodes_state_before_send_file_command
      @nodes.set.each { |node|
        node.last_cmd_exit_status = "0"
        node.last_cmd_stderr = ""
      }
    end

    def get_taktuk_send_file_command_infos(tw)
      tree = YAML.load((YAML.dump({"hosts"=>tw.hosts,
                                    "connectors"=>tw.connectors,
                                    "errors"=>tw.errors,
                                    "infos"=>tw.infos})))
      init_nodes_state_before_send_file_command
      tree['connectors'].each_value { |h|
        @nodes.get_node_by_host(h['peer']).last_cmd_exit_status = "256"
        @nodes.get_node_by_host(h['peer']).last_cmd_stderr = "The node #{h['peer']} is unreachable"
       }
    end

    def execute(cmd)
      tw = TaktukWrapper::new(make_taktuk_exec_cmd(cmd))
      tw.run
      get_taktuk_exec_command_infos(tw)
      good_nodes = Array.new
      bad_nodes = Array.new

      @nodes.set.each { |node|
        if node.last_cmd_exit_status == "0" then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]
    end

    def send_file(file, dest_dir)
      tw = TaktukWrapper::new(make_taktuk_send_file_cmd(file, dest_dir))
      tw.run
      get_taktuk_send_file_command_infos(tw)
      good_nodes = Array.new
      bad_nodes = Array.new
      @nodes.set.each { |node|
        if node.last_cmd_exit_status == "0" then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]   
    end
  end
end
