#Contrib libs
require 'taktuk_wrapper'

#Ruby libs
require 'yaml'
require 'socket'
require 'ping'

module ParallelOperations
  class ParallelOps
    @nodes = nil
    @taktuk_connector = nil
    @taktuk_tree_arity = nil
    @taktuk_auto_propagate = nil
    @output = nil
    @config = nil

    # Constructor of ParallelOps
    #
    # Arguments
    # * nodes: instance of NodeSet
    # * config: instance of Config
    # * taktuk_connector: specifies the connector to use with Taktuk
    # * output: OutputControl instance
    # Output
    # * nothing
    def initialize(nodes, config, taktuk_connector, output)
      @nodes = nodes
      @config = config
      @taktuk_connector = taktuk_connector
      @taktuk_tree_arity = config.common.taktuk_tree_arity
      @taktuk_auto_propagate = config.common.taktuk_auto_propagate
      @output = output
    end

    def make_taktuk_header_cmd
      args_tab = Array.new
      args_tab.push("-s") if @taktuk_auto_propagate
      if @taktuk_connector != "" then
        args_tab.push("-c")
        args_tab.push("#{@taktuk_connector}")
      end
      return args_tab
    end
    
    # Create a Taktuk string for an exec command
    #
    # Arguments
    # * cmd: command to execute
    # Output
    # * returns a string that contains the Taktuk command line for an exec command
    def make_taktuk_exec_cmd(cmd)
      args = String.new
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast exec [ #{cmd} ]"
      return make_taktuk_header_cmd + args.split(" ")
    end

    # Create a Taktuk string for a send file command
    #
    # Arguments
    # * file: file to send
    # * dest_dir: destination dir
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * returns a string that contains the Taktuk command line for a send file command
    def make_taktuk_send_file_cmd(file, dest_dir, scattering_kind)
      args = String.new
      case scattering_kind
      when "chain"
        args += " -d 1"
      when "tree"
        if (@taktuk_tree_arity > 0) then
          args += " -d #{@taktuk_tree_arity}"
        end
      else
        raise "Invalid structure for broadcasting file"
      end
      @nodes.make_sorted_array_of_nodes.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast put [ #{file} ] [ #{dest_dir} ]"
      return make_taktuk_header_cmd + args.split(" ")
    end

    # Create a Taktuk string for an exec command with an input file
    #
    # Arguments
    # * file: file to send as an input
    # * cmd: command to execute
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * returns a string that contains the Taktuk command line for an exec command with an input file
    def make_taktuk_exec_cmd_with_input_file(file, cmd, scattering_kind)
      args = String.new
      case scattering_kind
      when "chain"
        args += " -d 1"
      when "tree"
        if (@taktuk_tree_arity > 0) then
          args += " -d #{@taktuk_tree_arity}"
        end
      else
        raise "Invalid structure for broadcasting file"
      end
      @nodes.make_sorted_array_of_nodes.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast exec [ #{cmd} ];"
      args += " broadcast input file [ #{file} ]"
      return make_taktuk_header_cmd + args.split(" ")      
    end
 
    # Init a the state of a NodeSet before a send file command
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

    # Init a the state of a NodeSet before an exec command
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

    # Init a the state of a NodeSet before a reboot command
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def init_nodes_state_before_wait_nodes_after_reboot_command
      @nodes.set.each { |node|
        node.last_cmd_stderr = "The node #{node.hostname} is unreachable after the reboot"
        node.state = "KO"
      }
    end

    # Get the return information about an exec command with Taktuk
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
          @nodes.get_node_by_host(h['host_name']).last_cmd_stdout = x['output'].chomp
          @nodes.get_node_by_host(h['host_name']).last_cmd_stderr = x['error'].chomp
        }
      }
    end

    # Get the return information about a send file command with Taktuk
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

    # Get the return information about an exec command with an input file with Taktuk
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
          @nodes.get_node_by_host(h['host_name']).last_cmd_stdout = x['output'].chomp
          @nodes.get_node_by_host(h['host_name']).last_cmd_stderr = x['error'].chomp
        }
      }
    end

    # Execute a command in parallel
    #
    # Arguments
    # * cmd: command to execute
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)
    def execute(cmd)
      command_array = make_taktuk_exec_cmd(cmd)
      tw = TaktukWrapper::new(command_array)
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
      @output.debug("taktuk #{command_array.join(" ")}", @nodes)
      return [good_nodes, bad_nodes]
    end

    # Execute a command in parallel and expects some exit status
    #
    # Arguments
    # * cmd: command to execute
    # * status: array of expected exit status
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)
    def execute_expecting_status(cmd, status)
      command_array = make_taktuk_exec_cmd(cmd)
      tw = TaktukWrapper::new(command_array)
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
      @output.debug("taktuk #{command_array.join(" ")}", @nodes)
      return [good_nodes, bad_nodes]
    end

    # Execute a command in parallel and expects some exit status and an output
    #
    # Arguments
    # * cmd: command to execute
    # * status: array of expected exit status
    # * output: string that contains the expected output (only the first line is checked)
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)
    def execute_expecting_status_and_output(cmd, status, output)
      command_array = make_taktuk_exec_cmd(cmd)
      tw = TaktukWrapper::new(command_array)
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
      @output.debug("taktuk #{command_array.join(" ")}", @nodes)
      return [good_nodes, bad_nodes]
    end

    # Send a file in parallel
    #
    # Arguments
    # * file: file to send
    # * dest_dir: destination dir
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)    
    def send_file(file, dest_dir, scattering_kind)
      command_array = make_taktuk_send_file_cmd(file, dest_dir, scattering_kind)
      tw = TaktukWrapper::new(command_array)
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
      @output.debug("taktuk #{command_array.join(" ")}", @nodes)
      return [good_nodes, bad_nodes]   
    end

    # Execute a command in parallel with an input file
    #
    # Arguments
    # * file: file to send
    # * cmd: command to execute
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # * status: array of expected exit status
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)    
    def exec_cmd_with_input_file(file, cmd, scattering_kind, status)
      command_array = make_taktuk_exec_cmd_with_input_file(file, cmd, scattering_kind)
      tw = TaktukWrapper::new(command_array)
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
      @output.debug("taktuk #{command_array.join(" ")}", @nodes)
      return [good_nodes, bad_nodes]
    end

    # Wait for several nodes after a reboot command and wait a give time the effective reboot
    #
    # Arguments
    # * timeout: time to wait
    # * ports_up: array of ports that must be up on the rebooted nodes to test
    # * ports_down: array of ports that must be down on the rebooted nodes to test
    # * nodes_check_window: instance of WindowManager
    # Output
    # * returns an array that contains two arrays ([0] is the nodes OK and [1] is the nodes KO)    
    def wait_nodes_after_reboot(timeout, ports_up, ports_down, nodes_check_window)
      start = Time.now.tv_sec
      good_nodes = Array.new
      bad_nodes = Array.new
      init_nodes_state_before_wait_nodes_after_reboot_command
      @nodes.set.each { |node|
        @config.set_node_state(node.hostname, "", "", "reboot_in_progress")
      }
      sleep(20)

      while (((Time.now.tv_sec - start) < timeout) && (not @nodes.all_ok?))
        sleep(5)
        nodes_to_test = Nodes::NodeSet.new
        @nodes.set.each { |node|
          if node.state == "KO" then
            nodes_to_test.push(node)
          end
        }
        # We launch a thread here because a client SIGINT would corrupt the nodes check  window 
        # management, thus even a SIGINT is received, the reboot process will finish
        main_tid = Thread.new {
          callback = Proc.new { |ns|
            tg = ThreadGroup.new
            ns.set.each { |node|
              sub_tid = Thread.new {
                all_ports_ok = true
                if Ping.pingecho(node.hostname, 1, 22) then
                  ports_up.each { |port|
                    begin
                      s = TCPsocket.open(node.hostname, port)
                      s.close
                    rescue Errno::ECONNREFUSED
                      all_ports_ok = false
                      next
                    rescue Errno::EHOSTUNREACH
                      next
                    end
                  }
                  if all_ports_ok then
                    ports_down.each { |port|
                      begin
                        s = TCPsocket.open(node.hostname, port)
                        all_ports_ok = false
                        s.close
                      rescue Errno::ECONNREFUSED
                        next
                      rescue Errno::EHOSTUNREACH
                        next
                      end
                    }
                  end
                  if all_ports_ok then
                    node.state = "OK"
                    @output.verbosel(4, "  *** #{node.hostname} is here after #{Time.now.tv_sec - start}s")
                    @config.set_node_state(node.hostname, "", "", "rebooted")
                  else
                    node.state = "KO"
                  end
                end
              }
              tg.add(sub_tid)
            }
            #let's wait everybody
            tg.list.each { |sub_tid|
              sub_tid.join
            }
          }
          nodes_check_window.launch(nodes_to_test, &callback)
        }
        main_tid.join
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
