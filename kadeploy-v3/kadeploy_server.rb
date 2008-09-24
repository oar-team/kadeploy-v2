#!/usr/bin/ruby -w
require 'drb'
require 'lib/managers'
require 'lib/debug'

class KadeployServer
  @config = nil
  @client = nil
  attr_reader :mutex
  attr_reader :tcp_buffer_size
  attr_reader :dest_host
  attr_reader :dest_port
  @file_name = nil #any access to file_name must be protected with mutex

  # Constructor of KadeployServer
  #
  # Arguments
  # * config: instance of Config
  # Output
  # * raises an exception if the file server can not open a socket
  def initialize(config)
    @config = config
    @dest_host = @config.common.kadeploy_server
    @dest_port = @config.common.kadeploy_file_server_port
    @tcp_buffer_size = @config.common.kadeploy_tcp_buffer_size
    puts "Launching the Kadeploy file server"
    @mutex = Mutex.new
    sock = TCPServer.open(@dest_host, @dest_port)

#    sock = Socket.new(Socket::AF_INET, Socket::SOCK_STREAM, 0)
#    opt = [1].pack("i")
#    sock.setsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR, opt)
#    sockaddr = Socket.pack_sockaddr_in(@dest_port, @dest_host)
#    sock.bind(sockaddr)
#    sock.listen(10)

    if (sock.kind_of? TCPSocket) then
      Thread.new {
        while (session = sock.accept)
          file = File.new(@config.common.kadeploy_cache_dir + "/" + @file_name, "w")
          while ((buf = session.recv(@tcp_buffer_size)) != "") do
            file.write(buf)
          end
          file.close
          session.close
        end
      }
    else
      raise "Can not open a socket on port #{@dest_port}"
    end
  end

  # Prevents a shared object related to the file transfers from any modifications, it must be called before a file transfer (RPC)
  #
  # Arguments
  # * file_name: name of the file that will be transfered
  # Output
  # * nothing
  def pre_send_file(file_name)
    @mutex.lock
    @file_name = file_name
  end

  # Releases a lock on a shared object, it must be called after a file transfer (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * nothing
  def post_send_file
    @mutex.unlock
  end

  # Gets the common configuration (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * nothing
  def get_common_config
    return @config.common
  end

  # Gets the default deployment partition (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * nothing
  def get_default_deploy_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].deploy_part
  end


  # Gets the production partition (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * nothing
  def get_prod_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].prod_part
  end

  # Sets the exec_specific configuration from the client side (RPC)
  #
  # Arguments
  # * exec_specific: instance of Config.exec_specific
  # Output
  # * nothing
  def set_exec_specific_config(exec_specific)
    @config.exec_specific = exec_specific
    #overide the configuration if the steps are specified in the command line
    if (not @config.exec_specific.steps.empty?) then
      @config.exec_specific.node_list.group_by_cluster.each_key { |cluster|
        @config.cluster_specific[cluster].workflow_steps = @config.exec_specific.steps
      }
    end
  end

  # Launches the workflow from the client side (RPC)
  #
  # Arguments
  # * host: hostname of the client
  # * port: port on which the client listen to Drb
  # Output
  # * nothing
  def launch_workflow(host, port)
    puts "Let's launch an instance of Kadeploy"
    DRb.start_service()
    uri = "druby://#{host}:#{port}"
    client = DRbObject.new(nil, uri)
    workflow=Managers::WorkflowManager.new(@config, client)
    workflow.run
  end

  # Reboot a set of nodes from the client side (RPC)
  #
  # Arguments
  # * host: hostname of the client
  # * port: port on which the client listen to Drb
  # Output
  # * nothing  
  def launch_reboot(node_list, partition, host, port, debug_level)
    DRb.start_service()
    uri = "druby://#{host}:#{port}"
    client = DRbObject.new(nil, uri)
    if (debug_level != nil) then
      dl = debug_level
    else
      dl = @config.common.debug_level
    end
    output = Debug::OutputControl.new(dl, client)
    node_list.group_by_cluster.each_pair { |cluster, set|
      step = MicroStepsLibrary::MicroSteps.new(set, Nodes::NodeSet.new, nil, @config, cluster, output)
      if (partition == "") then
        step.switch_pxe("back_to_prod_env")
        step.reboot("soft")
      else
        step.reboot("hard")
      end
      step.wait_reboot(@config.common.ssh_port)
    }
  end
end


Signal.trap("INT") do
  puts "SIGINT trapped, let's clean everything ..."
  #todo: clean some stuff
  exit 1
end

config = ConfigInformation::Config.new("kadeploy")
if (config.check_config("kadeploy") == true)
  puts "Launching the Kadeploy RPC server"
  uri = "druby://#{config.common.kadeploy_server}:#{config.common.kadeploy_server_port}"
  kadeployServer = KadeployServer.new(config)
  DRb.start_service(uri, kadeployServer)
  DRb.thread.join
else
  puts "Bad configuration"
end
