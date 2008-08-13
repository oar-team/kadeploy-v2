#!/usr/bin/ruby -w

#Kadeploy libs
require 'lib/debug'
require 'lib/nodes'
require 'lib/config'
require 'lib/managers'
require 'lib/stepdeployenv'
require 'lib/stepbroadcastenv'
require 'lib/stepbootnewenv'

#Ruby libs
require 'thread'
require 'drb'

class KadeployWorkflow
  @thread_set_deployment_environment = nil
  @thread_broadcast_environment = nil
  @thread_boot_new_environment = nil 
  @thread_process_finished_nodes = nil
  @queue_manager = nil
  @output = nil
  @rights = nil
  @nodeset = nil
  @config = nil
  @client = nil
  @window_manager = nil
  attr_accessor :nodes_ok
  attr_accessor :nodes_ko

  def initialize(config, client)
    @config = config
    @client = client
    @output = Debug::OutputControl.new(@config.common.debug_level, client)
    @nodes_ok = Nodes::NodeSet.new
    @nodes_ko = Nodes::NodeSet.new
    @nodeset = @config.exec_specific.node_list
    @queue_manager = Managers::QueueManager.new(@config, @nodes_ok, @nodes_ko)
    @window_manager = Managers::WindowManager.new

    @thread_set_deployment_environment = Thread.new {
      launch_thread_for_macro_step("SetDeploymentEnv")
    }
    @thread_broadcast_environment = Thread.new {
      launch_thread_for_macro_step("BroadcastEnv")
    }
    @thread_boot_new_environment = Thread.new {
      launch_thread_for_macro_step("BootNewEnv")
    }
    @thread_process_finished_nodes = Thread.new {
      launch_thread_for_macro_step("ProcessFinishedNodes")
    }
  end
  
  def launch_thread_for_macro_step(kind)
    close_thread = false
    @output.debugl(4, "#{kind} thread launched")
    while (not close_thread) do
      nodes = @queue_manager.get_task(kind)
      #We receive the signal to exit
      if (nodes.kind_of?(Managers::MagicCookie)) then
        close_thread = true
      else
        if kind != "ProcessFinishedNodes" then
          nodes.group_by_cluster.each_pair { |cluster, set|
            macro_step_instance = @config.cluster_specific[cluster].get_macro_step(kind).get_instance
            instance_name = macro_step_instance[0]
            instance_max_retries = macro_step_instance[1]
            case kind
            when "SetDeploymentEnv"
              SetDeploymentEnvironnment::SetDeploymentEnvFactory.create(instance_name, 
                                                                        instance_max_retries,
                                                                        cluster,
                                                                        set,
                                                                        @queue_manager,
                                                                        @window_manager,
                                                                        @output).run
            when "BroadcastEnv"
              BroadcastEnvironment::BroadcastEnvFactory.create(instance_name, 
                                                               instance_max_retries, 
                                                               cluster,
                                                               set,
                                                               @queue_manager,
                                                               @window_manager,
                                                               @output).run
            when "BootNewEnv"
              BootNewEnvironment::BootNewEnvFactory.create(instance_name, 
                                                           instance_max_retries, 
                                                           cluster,
                                                           set,
                                                           @queue_manager,
                                                           @window_manager,
                                                           @output).run
            else
              raise "Invalid macro step name"
            end
          }
        else
          #in this case, all is ok
          if not nodes.empty? then
            @nodes_ok.add(nodes)
          end
          if @queue_manager.one_last_active_thread? then
            @nodes_ok.group_by_cluster.each_pair { |cluster, set|
              @output.debugl(0, "Nodes correctly deployed on cluster #{cluster}")
              @output.debugl(0, set.to_s)
            }
            @nodes_ko.group_by_cluster.each_pair { |cluster, set|
              @output.debugl(0, "Nodes not Correctly deployed on cluster #{cluster}")
              @output.debugl(0, set.to_s(true))
            }
            @queue_manager.send_exit_signal
            @thread_set_deployment_environment.join
            @thread_broadcast_environment.join
            @thread_boot_new_environment.join
          end
        end
      end
    end
  end

  def grab_user_files
    @output.debugl(4, "Grab the tarball file #{@config.exec_specific.environment.tarball_file}")
    @client.get_file(@config.exec_specific.environment.tarball_file)
  end

  def run
    @output.debugl(0, "Launching Kadeploy ...")
    grab_user_files
    @queue_manager.next_macro_step(nil, @nodeset)
    @thread_process_finished_nodes.join
  end
end

class KadeployServer
  @config = nil
  @client = nil
  attr_reader :mutex
  attr_reader :tcp_buffer_size
  attr_reader :dest_host
  attr_reader :dest_port
  @file_name = nil #any access to file_name must be protected with mutex

  def initialize(config)
    @config = config
    @dest_host = @config.common.kadeploy_server
    @dest_port = @config.common.kadeploy_file_server_port
    @tcp_buffer_size = @config.common.kadeploy_tcp_buffer_size
    puts "Launching the Kadeploy file server"
    @mutex = Mutex.new
    sock = TCPServer.open(@dest_host, @dest_port)
    if (sock.kind_of? TCPSocket) then
      Thread.new {
        while (session = sock.accept)
          file = File.new(@config.common.kadeploy_cache_dir + "/" + @file_name, "w")
          while (buf = session.recvfrom(@tcp_buffer_size)[0])
            file.write(buf)
          end
        end
      }
    else
      raise "Can not open a socket on port #{@config.file_port}"
    end
  end

  def pre_send_file(file_name)
    @mutex.lock
    @file_name = file_name
  end

  def post_send_file
    @mutex.unlock
  end

  def get_common_config
    return @config.common
  end

  def get_default_deploy_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].deploy_part
  end

  def set_exec_specific_config(exec_specific)
    @config.exec_specific = exec_specific
  end

  def launch_workflow(host, port)
    puts "Let's launch an instance of Kadeploy"
    DRb.start_service()
    uri = "druby://#{host}:#{port}"
    client = DRbObject.new(nil, uri)
    workflow=KadeployWorkflow.new(@config, client)
    workflow.run
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
