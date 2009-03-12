#!/usr/bin/ruby -w

#Kadeploy libs
require 'managers'
require 'debug'
require 'microsteps'

#Ruby libs
require 'drb'

class KadeployServer
  @config = nil
  @client = nil
  attr_reader :file_server_lock
  attr_reader :deployments_table_lock
  attr_reader :tcp_buffer_size
  attr_reader :dest_host
  attr_reader :dest_port
  @db = nil
  @file_name = nil #any access to file_name must be protected with file_server_lock
  @reboot_window = nil
  @nodes_check_window = nil
  @syslog_lock = nil
  @workflow_hash = nil
  @workflow_hash_lock = nil
  @workflow_hash_index = nil

  # Constructor of KadeployServer
  #
  # Arguments
  # * config: instance of Config
  # * reboot_window: instance of WindowManager to manage the reboot window
  # * nodes_check_window: instance of WindowManager to manage the check of the nodes
  # * db: database handler
  # Output
  # * raises an exception if the file server can not open a socket
  def initialize(config, reboot_window, nodes_check_window, db)
    @config = config
    @dest_host = @config.common.kadeploy_server
    @dest_port = @config.common.kadeploy_file_server_port
    @tcp_buffer_size = @config.common.kadeploy_tcp_buffer_size
    @reboot_window = reboot_window
    @nodes_check_window = nodes_check_window
    puts "Launching the Kadeploy file server"
    @file_server_lock = Mutex.new
    @deployments_table_lock = Mutex.new
    @syslog_lock = Mutex.new
    sock = TCPServer.open(@dest_host, @dest_port)
    @db = db
    @workflow_info_hash = Hash.new
    @workflow_info_hash_lock = Mutex.new
    @workflow_info_hash_index = 0
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

  # Record a Managers::WorkflowManager pointer
  #
  # Arguments
  # * workflow_ptr: reference toward a Managers::WorkflowManager
  # Output
  # * return an id that allows to find the right Managers::WorkflowManager reference
  def add_workflow_info(workflow_ptr)
    @workflow_info_hash_lock.lock
    id = @workflow_info_hash_index
    @workflow_info_hash[id] = workflow_ptr
    @workflow_info_hash_index += 1
    @workflow_info_hash_lock.unlock
    return id
  end

  # Prevent a shared object related to the file transfers from any modifications, it must be called before a file transfer (RPC)
  #
  # Arguments
  # * file_name: name of the file that will be transfered
  # Output
  # * nothing
  def pre_send_file(file_name)
    @file_server_lock.lock
    @file_name = file_name
  end

  def kill_instance(id)
    workflow = @workflow_info_hash[id]
    workflow.kill_instance()
    @workflow_info_hash.delete(id)
  end

  # Release a lock on a shared object, it must be called after a file transfer (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * nothing
  def post_send_file
    @file_server_lock.unlock
  end

  # Get the common configuration (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * nothing
  def get_common_config
    return @config.common
  end

  # Get the default deployment partition (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * nothing
  def get_default_deploy_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].deploy_part
  end


  # Get the production partition (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * nothing
  def get_prod_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].prod_part
  end

  # Set the exec_specific configuration from the client side (RPC)
  #
  # Arguments
  # * exec_specific: instance of Config.exec_specific
  # Output
  # * nothing
#   def set_exec_specific_config(exec_specific)
#     @config.exec_specific = exec_specific
#     #overide the configuration if the steps are specified in the command line
#     if (not @config.exec_specific.steps.empty?) then
#       @config.exec_specific.node_list.group_by_cluster.each_key { |cluster|
#         @config.cluster_specific[cluster].workflow_steps = @config.exec_specific.steps
#       }
#     end
#   end

  # Launch the workflow from the client side (RPC)
  #
  # Arguments
  # * host: hostname of the client
  # * port: port on which the client listen to Drb
  # * exec_specific: instance of Config.exec_specific
  # Output
  # * nothing
  def launch_workflow(host, port, exec_specific)
    puts "Let's launch an instance of Kadeploy"
    DRb.start_service()
    uri = "druby://#{host}:#{port}"
    client = DRbObject.new(nil, uri)

    #We create a new instance of Config with a specific exec_specific part
    config = ConfigInformation::Config.new("empty")
    config.common = @config.common.clone
    config.cluster_specific = @config.cluster_specific.clone
    config.exec_specific = exec_specific
    #overide the configuration if the steps are specified in the command line
    if (not exec_specific.steps.empty?) then
      exec_specific.node_list.group_by_cluster.each_key { |cluster|
        config.cluster_specific[cluster].workflow_steps = exec_specific.steps
      }
    end

    workflow = Managers::WorkflowManager.new(config, client, @reboot_window, @nodes_check_window, @db, @deployments_table_lock, @syslog_lock)
    client.set_workflow_id(add_workflow_info(workflow))
    workflow.run
    #let's free memory at the end of the workflow
    GC.start
  end

  # Reboot a set of nodes from the client side (RPC)
  #
  # Arguments
  # * exec_specific: instance of Config.exec_specific
  # * host: hostname of the client
  # * port: port on which the client listen to Drb
  # Output
  # * return 0 in case of success, 1 if the reboot failed on some nodes, 2 if the reboot has not been launched
  def launch_reboot(exec_specific, host, port, debug_level, pxe_profile_msg)
    DRb.start_service()
    uri = "druby://#{host}:#{port}"
    client = DRbObject.new(nil, uri)
    return_value = 0
    if (debug_level != nil) then
      dl = debug_level
    else
      dl = @config.common.debug_level
    end
    @config.common.taktuk_connector = @config.common.taktuk_ssh_connector
    output = Debug::OutputControl.new(dl, client, exec_specific.true_user, -1, 
                                      @config.common.dbg_to_syslog, @config.common.dbg_to_syslog_level, @syslog_lock)
    if (exec_specific.reboot_kind == "back_to_prod_env") && 
        exec_specific.check_prod_env && 
        exec_specific.node_list.check_demolishing_env(@db, @config.common.demolishing_env_threshold) then
      output.debugl(0, "Reboot not performed since some nodes have been deployed with a demolishing environment")
      return_value = 2
    else
      #We create a new instance of Config with a specific exec_specific part
      config = ConfigInformation::Config.new("empty")
      config.common = @config.common.clone
      config.cluster_specific = @config.cluster_specific.clone
      config.exec_specific = exec_specific
      exec_specific.node_list.group_by_cluster.each_pair { |cluster, set|
        step = MicroStepsLibrary::MicroSteps.new(set, Nodes::NodeSet.new, @reboot_window, @nodes_check_window, config, cluster, output, "Kareboot")
        case exec_specific.reboot_kind
        when "back_to_prod_env"
          step.switch_pxe("back_to_prod_env")
        when "set_pxe"
          step.switch_pxe("set_pxe", pxe_profile_msg)
        when "simple_reboot"
          #no need to change the PXE profile
        else
          raise "Invalid kind of reboot: #{@reboot_kind}"
        end
        step.reboot("soft")
        step.wait_reboot([@config.common.ssh_port],[])
        if (exec_specific.reboot_kind == "back_to_prod_env") then
          set.set_deployment_state("prod_env", nil, @db)
          step.check_nodes("prod_env_booted")
          if (exec_specific.check_prod_env) then
            step.nodes_ko.tag_demolishing_env(@db)
            return_value = 1
          end
        end
      }
    end
    return return_value
  end
end

config = ConfigInformation::Config.new("kadeploy")
if (config.check_config("kadeploy") == true)
  if config.common.use_local_bt_tracker then
    Thread.new {
      puts "Launching the Bittorrent tracker"
      res = system("bttrack --port #{config.common.bt_tracker_port} --dfile #{config.common.kadeploy_cache_dir}/bt_download_state &>/dev/null")
    }
  end
  db = Database::DbFactory.create(config.common.db_kind)
  Signal.trap("INT") do
    puts "SIGINT trapped, let's clean everything ..."
    db.disconnect
    exit 1
  end
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)
  kadeployServer = KadeployServer.new(config, 
                                      Managers::WindowManager.new(config.common.reboot_window, config.common.reboot_window_sleep_time),
                                      Managers::WindowManager.new(config.common.nodes_check_window, 1),
                                      db)
  puts "Launching the Kadeploy RPC server"
  uri = "druby://#{config.common.kadeploy_server}:#{config.common.kadeploy_server_port}"
  DRb.start_service(uri, kadeployServer)
  DRb.thread.join
else
  puts "Bad configuration"
end
