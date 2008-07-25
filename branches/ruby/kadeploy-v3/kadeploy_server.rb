#!/usr/bin/ruby -w

#Kadeploy libs
require 'lib/debug'
require 'lib/checkrights'
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
    @output = Debug::OutputControl.new(@config.common.debug_level)
    @rights = CheckRights::CheckRightsFactory.new(@config.common.rights_kind).klass
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
              puts "Nodes correctly deployed on cluster #{cluster}"
              puts set.to_s
            }
            @nodes_ko.group_by_cluster.each_pair { |cluster, set|
              puts "Nodes not Correctly deployed on cluster #{cluster}"
              puts set.to_s(true)
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

  def run
    puts "Launching Kadeploy ..."
    if (@rights.granted? == true)
      @queue_manager.next_macro_step(nil, @nodeset)
      @thread_process_finished_nodes.join
    else
      puts "You do not have the deployment rights on all the nodes"
    end
  end
end

class KadeployServer
  @config = nil
  @client = nil
  
  def initialize(config)
    @config = config
  end

  def get_common_config
    return @config.common
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
  kadeployServer = KadeployServer.new(config)
  uri = "druby://#{config.common.kadeploy_server}:#{config.common.kadeploy_server_port}"
  DRb.start_service(uri, kadeployServer)
  DRb.thread.join
else
  puts "Bad configuration"
end
