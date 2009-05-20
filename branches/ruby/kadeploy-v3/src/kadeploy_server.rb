#!/usr/bin/ruby -w

#Kadeploy libs
require 'managers'
require 'debug'
require 'microsteps'
require 'process_management'

#Ruby libs
require 'drb'
require 'socket'

class KadeployServer
  @config = nil
  @client = nil
 # attr_reader :file_server_lock
  attr_reader :deployments_table_lock
  attr_reader :tcp_buffer_size
  attr_reader :dest_host
  attr_reader :dest_port
  @db = nil
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
    @tcp_buffer_size = @config.common.kadeploy_tcp_buffer_size
    @reboot_window = reboot_window
    @nodes_check_window = nodes_check_window
    puts "Launching the Kadeploy file server"
    @deployments_table_lock = Mutex.new
    @syslog_lock = Mutex.new
    @db = db
    @workflow_info_hash = Hash.new
    @workflow_info_hash_lock = Mutex.new
    @workflow_info_hash_index = 0
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

  # Create a socket server designed to copy a file from to client to the server cache (RPC)
  #
  # Arguments
  # * filename: name of the destination file
  # Output
  # * return the port allocated to the socket server
  def create_a_socket_server(filename)
    sock = Socket.new(Socket::AF_INET, Socket::SOCK_STREAM, 0)
    sockaddr = Socket.pack_sockaddr_in(0, @config.common.kadeploy_server)
    begin
      sock.bind(sockaddr)
    rescue
      return -1
    end
    port = Socket.unpack_sockaddr_in(sock.getsockname)[0].to_i
    Thread.new {
      sock.listen(10)
      begin
        session = sock.accept
        file = File.new(@config.common.kadeploy_cache_dir + "/" + filename, "w")
        while ((buf = session[0].recv(@tcp_buffer_size)) != "") do
          file.write(buf)
        end
        file.close
        session[0].close
      rescue
        puts "The client has been probably disconnected..."
      end
    }
    return port
  end

  # Kill a workflow (RPC)
  #
  # Arguments
  # * id: id of the workflow
  # Output
  # * nothing  
  def kill(id)
    # id == -1 means that the workflow has not been launched yet
    if (id != -1) then
      workflow = @workflow_info_hash[id]
      workflow.kill_workflow()
      @workflow_info_hash.delete(id)
    end
  end

  # Get the common configuration (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * return a CommonConfig instance
  def get_common_config
    return @config.common
  end

  # Get the default deployment partition (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * return the name of the default deployment partition
  def get_default_deploy_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].deploy_part
  end

  # Get the block device (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * return the name of the block device
  def get_block_device(cluster)
    return @config.cluster_specific[cluster].block_device
  end

  # Get the production partition (RPC)
  #
  # Arguments
  # * cluster: name of the cluster concerned
  # Output
  # * return the production partition
  def get_prod_part(cluster)
    return @config.cluster_specific[cluster].block_device + @config.cluster_specific[cluster].prod_part
  end


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
    config.common = @config.common
    config.exec_specific = exec_specific
    config.cluster_specific = Hash.new
    #Overide the configuration if the steps are specified in the command line
    if (not exec_specific.steps.empty?) then
      exec_specific.node_list.group_by_cluster.each_key { |cluster|
        config.cluster_specific[cluster] = ConfigInformation::ClusterSpecificConfig.new
        @config.cluster_specific[cluster].duplicate_but_steps(config.cluster_specific[cluster], exec_specific.steps)
      }
    #If the environment specifies a preinstall, we override the automata to use specific preinstall
    elsif (exec_specific.environment.preinstall != nil) then
      puts "A specific presinstall will be used with this environment"
      exec_specific.node_list.group_by_cluster.each_key { |cluster|
        config.cluster_specific[cluster] = ConfigInformation::ClusterSpecificConfig.new
        @config.cluster_specific[cluster].duplicate_all(config.cluster_specific[cluster])
        instance = config.cluster_specific[cluster].get_macro_step("SetDeploymentEnv").get_instance
        max_retries = instance[1]
        timeout = instance[2]
        config.cluster_specific[cluster].replace_macro_step("SetDeploymentEnv", ["SetDeploymentEnvUntrustedCustomPreInstall", max_retries, timeout])
      }
    else
      config.cluster_specific = @config.cluster_specific
    end

    #We check if the depoyment involves several clusters (useful for printing purposes)
    config.exec_specific.multicluster = (exec_specific.node_list.get_nb_clusters > 1)

    workflow = Managers::WorkflowManager.new(config, client, @reboot_window, @nodes_check_window, @db, @deployments_table_lock, @syslog_lock)
    workflow_id = add_workflow_info(workflow)
    client.set_workflow_id(workflow_id)
    finished = false
    tid = Thread.new {
      while (not finished) do
        begin
          client.test()
        rescue DRb::DRbConnError
          workflow.output.disable_client_output()
          workflow.output.debugl(4, "Client disconnection")
          workflow.kill_workflow()
          finished = true
        end
        sleep(1)
      end
    }
    workflow.run
    finished = true
    #let's free memory at the end of the workflow
    tid = nil
    @workflow_info_hash.delete(workflow_id)
    workflow.finalize
    config.cluster_specific = nil
    config = nil
    workflow = nil
    exec_specific = nil
    client = nil
    DRb.stop_service()
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
        when "deploy_env"
          step.switch_pxe("prod_to_deploy_env")
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
        if (exec_specific.reboot_kind == "deploy_env") then
          step.send_key_in_deploy_env("tree")
        end
      }
      config = nil
    end
    return return_value
  end
end



begin
  config = ConfigInformation::Config.new("kadeploy")
rescue
  puts "Bad configuration"
  exit(1)
end
if (config.check_config("kadeploy") == true)
  db = Database::DbFactory.create(config.common.db_kind)
  Signal.trap("TERM") do
    puts "TERM trapped, let's clean everything ..."
    db.disconnect
    exit(1)
  end
  Signal.trap("INT") do
    puts "SIGINT trapped, let's clean everything ..."
    db.disconnect
    exit(1)
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
