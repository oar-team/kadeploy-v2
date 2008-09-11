require 'contrib/taktuk_wrapper'
require 'yaml'
require 'socket'

module ParallelOperations
  class ParallelOps
    @nodes = nil
    @taktuk_connector = nil
    @taktuk_tree_arity = nil
    @taktuk_auto_propagate = nil

    # Constructors of ParallelOps
    #
    # Arguments
    # * nodes: instance of NodeSet
    # * config: instance of Config
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * nothing
    def initialize(nodes, config, taktuk_connector)
      @nodes = nodes
      @taktuk_connector = taktuk_connector
      @taktuk_tree_arity = config.common.taktuk_tree_arity
      @taktuk_auto_propagate = config.common.taktuk_auto_propagate
    end

    # Creates a Taktuk string for an exec command
    #
    # Arguments
    # * cmd: command to execute
    # Output
    # * returns a string that contains the Taktuk command line for an exec command
    def make_taktuk_exec_cmd(cmd)
      args = String.new
      args += " -s" if @taktuk_auto_propagate
      args += " -c #{@taktuk_connector}" if @taktuk_connector != ""
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast exec [ #{cmd} ]"
      return args.split(" ")
    end

    # Creates a Taktuk string for a send file command
    #
    # Arguments
    # * file: file to send
    # * dest_dir: destination dir
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * returns a string that contains the Taktuk command line for a send file command
    def make_taktuk_send_file_cmd(file, dest_dir, scattering_kind)
      args = String.new
      args += " -s" if @taktuk_auto_propagate
      args += " -c #{@taktuk_connector}" if @taktuk_connector != ""
      case scattering_kind
      when "chain"
        args += " -d 1"
      when "tree"
        args += " -d #{@taktuk_tree_arity}"
      else
        raise "Invalid structure for broadcasting file"
      end
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast put [ #{file} ] [ #{dest_dir} ]"
      return args.split(" ")
    end

    # Creates a Taktuk string for an exec command with an input file
    #
    # Arguments
    # * file: file to send as an input
    # * cmd: command to execute
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * returns a string that contains the Taktuk command line for an exec command with an input file
    def make_taktuk_exec_cmd_with_input_file(file, cmd, scattering_kind)
      args = String.new
      args += " -s" if @taktuk_auto_propagate
      args += " -c #{@taktuk_connector}" if @taktuk_connector != ""
      case scattering_kind
      when "chain"
        args += " -d 1"
      when "tree"
        args += " -d #{@taktuk_tree_arity}"
      else
        raise "Invalid structure for broadcasting file"
      end
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast exec [ #{cmd} ];"
      args += " broadcast input file [ #{file} ]"
      return args.split(" ")      
    end
 
    # Inits a the state of a NodeSet before a send file command
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def init_nodes_state_before_send_file_command
      @nodes.set.each { |node|
        node.last_cmd_exit_status = "0"
        node.last_cmd_stderr = ""
      }
    end

    # Inits a the state of a NodeSet before an exec command
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def init_nodes_state_before_exec_command
      @nodes.set.each { |node|
        node.last_cmd_exit_status = "256"
        node.last_cmd_stderr = "The node #{node.hostname} is unreachable"
      }
    end

    # Inits a the state of a NodeSet before a reboot command
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def init_nodes_state_before_wit_nodes_after_reboot_command
      @nodes.set.each { |node|
        node.last_cmd_stderr = "The node #{node.hostname} is unreachable after the reboot"
        node.state = "KO"
      }
    end

    # Gets the return information about an exec command with Taktuk
    #
    # Arguments
    # * tw: instance of TaktukWrapper
    # Output
    # * nothing
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

    # Gets the return information about a send file command with Taktuk
    #
    # Arguments
    # * tw: instance of TaktukWrapper
    # Output
    # * nothing
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

    # Gets the return information about an exec command with an input file with Taktuk
    #
    # Arguments
    # * tw: instance of TaktukWrapper
    # Output
    # * nothing
    def get_taktuk_exec_cmd_with_input_file_infos(tw)
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

    # Executes a command in parallel
    #
    # Arguments
    # * cmd: command to execute
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)
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

    # Executes a command in parallel and expects some exit status
    #
    # Arguments
    # * cmd: command to execute
    # * status: array of expected exit status
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)
    def execute_expecting_status(cmd, status)
      tw = TaktukWrapper::new(make_taktuk_exec_cmd(cmd))
      tw.run
      get_taktuk_exec_command_infos(tw)
      good_nodes = Array.new
      bad_nodes = Array.new

      @nodes.set.each { |node|
        if status.include?(node.last_cmd_exit_status) then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]
    end

    # Executes a command in parallel and expects some exit status and an output
    #
    # Arguments
    # * cmd: command to execute
    # * status: array of expected exit status
    # * output: string that contains the expected output (only the first line is checked)
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)
    def execute_expecting_status_and_output(cmd, status, output)
      tw = TaktukWrapper::new(make_taktuk_exec_cmd(cmd))
      tw.run
      get_taktuk_exec_command_infos(tw)
      good_nodes = Array.new
      bad_nodes = Array.new

      @nodes.set.each { |node|
        if (status.include?(node.last_cmd_exit_status) == true) && 
            (node.last_cmd_stdout.split("\n")[0] == output) then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]
    end

    # Sends a file in parallel
    #
    # Arguments
    # * file: file to send
    # * dest_dir: destination dir
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)    
    def send_file(file, dest_dir, scattering_kind)
      tw = TaktukWrapper::new(make_taktuk_send_file_cmd(file, dest_dir, scattering_kind))
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

    # Executes a command in parallel with an input file
    #
    # Arguments
    # * file: file to send
    # * cmd: command to execute
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # * status: array of expected exit status
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)    
    def exec_cmd_with_input_file(file, cmd, scattering_kind, status)
      tw = TaktukWrapper::new(make_taktuk_exec_cmd_with_input_file(file, cmd, scattering_kind))
      tw.run
      get_taktuk_exec_cmd_with_input_file_infos(tw)
      good_nodes = Array.new
      bad_nodes = Array.new
      @nodes.set.each { |node|
        if node.last_cmd_exit_status == status then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]
    end

    # Wait for several nodes after a reboot command and wait a give time the effective reboot
    #
    # Arguments
    # * timeout: time to wait
    # * port: port probed on the rebooted nodes to test
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)    
    def wait_nodes_after_reboot(timeout, port)
      good_nodes = Array.new
      bad_nodes = Array.new
      init_nodes_state_before_wit_nodes_after_reboot_command
      sleep 10
      start = Time.now.tv_sec
      while (((Time.now.tv_sec - start) < timeout) && (not @nodes.all_ok?))
        sleep 2
        @nodes.set.each { |node|
          if node.state == "KO" then
            sock = TCPSocket.new(node.hostname, port) rescue false
            if sock.kind_of? TCPSocket then
              node.state = "OK"
            end
          end
        }
      end
      @nodes.set.each { |node|
        if node.state == "OK" then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]
    end
  end
end
