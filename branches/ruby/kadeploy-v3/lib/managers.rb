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

module Managers
  class MagicCookie
  end

  class WindowManager
    @mutex = nil
    @resources_used = nil
    @resources_max = nil
    @sleep_time = nil

    # Constructor of WindowManager
    #
    # Arguments
    # * max: max size of the window
    # * sleep_time: sleeping time before releasing resources
    # Output
    # * nothing
    def initialize(max, sleep_time)
      @mutex = Mutex.new
      @resources_used = 0
      @resources_max = max
      @sleep_time = sleep_time
    end

    private
    # Try to acquire a given number of resources
    #
    # Arguments
    # * n: number of resources to acquire
    # Output
    # * returns two values: the number of resources not acquires and the number of taken resources
    def acquire(n)
      @mutex.synchronize {
        remaining_resources = @resources_max - @resources_used
        if (remaining_resources == 0) then
          return 0, 0
        else
          if (n <= remaining_resources) then
            @resources_used += n
            return 0, n
          else
            not_acquired = n - remaining_resources
            @resources_used = @resources_max
            return not_acquired, remaining_resources
          end
        end
      }
    end

    # Release a given number of resources
    #
    # Arguments
    # * n: number of resources to release
    # Output
    # * nothing
    def release(n)
      @mutex.synchronize {
        @resources_used -= n
      }
    end

    public
    # Launch a windowed reboot
    #
    # Arguments
    # * node_set: instance of NodeSet that contains the set of nodes to reboot
    # * callback: reference on block that takes a NodeSet as argument
    # Output
    # * nothing
    def launch_reboot(node_set, &callback)
      remaining = node_set.length
      while (remaining != 0)
        remaining, taken = acquire(remaining)
        if (taken > 0) then
          partial_set = node_set.extract(taken)
          callback.call(partial_set)
          release(taken)
        end
        sleep(@sleep_time) if remaining != 0
      end
    end
  end

  class QueueManager
    @queue_deployment_environment = nil
    @queue_broadcast_environment = nil
    @queue_boot_new_environment = nil
    @queue_process_finished_nodes = nil
    attr_reader :config
    @nodes_ok = nil
    @nodes_ko = nil
    @mutex = nil
    attr_accessor :nb_active_threads

    # Constructor of QueueManager
    #
    # Arguments
    # * config: instance of Config
    # * nodes_ok: NodeSet of nodes OK
    # * nodes_ko: NodeSet of nodes KO
    # Output
    # * nothing
    def initialize(config, nodes_ok, nodes_ko)
      @config = config
      @nodes_ok = nodes_ok
      @nodes_ko = nodes_ko
      @mutex = Mutex.new
      @nb_active_threads = 0
      @queue_deployment_environment = Queue.new
      @queue_broadcast_environment = Queue.new
      @queue_boot_new_environment = Queue.new
      @queue_process_finished_nodes = Queue.new
    end

    # Increment the number of active threads
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def increment_active_threads
      @mutex.synchronize {
        @nb_active_threads += 1
      }
    end

    # Decrement the number of active threads
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def decrement_active_threads
      @mutex.synchronize {
        @nb_active_threads -= 1
      }
    end

    # Test if the there is only one active thread
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if there is only one active thread
    def one_last_active_thread?
      @mutex.synchronize {
        return (@nb_active_threads == 1)
      }
    end

    # Go to the next macro step in the automata
    #
    # Arguments
    # * current: name of the current macro step (SetDeploymentEnv, BroadcastEnv, BootNewEnv)
    # * nodes: NodeSet that must be involved in the next step
    # Output
    # * raises an exception if a wrong step name is given
    def next_macro_step(current_step, nodes)
      if (nodes.set.empty?)
        raise "Empty node set"
      else
        case current_step
        when nil
          @queue_deployment_environment.push(nodes)
        when "SetDeploymentEnv"
          @queue_broadcast_environment.push(nodes)
        when "BroadcastEnv"
          @queue_boot_new_environment.push(nodes)
        when "BootNewEnv"
          @queue_process_finished_nodes.push(nodes)
        else
          raise "Wrong step name"
        end
      end
    end

    # Replay a step with another instance
    #
    # Arguments
    # * current: name of the current macro step (SetDeploymentEnv, BroadcastEnv, BootNewEnv)
    # * cluster: name of the cluster whose the nodes belongs
    # * nodes: NodeSet that must be involved in the replay
    # Output
    # * returns true if the step can be replayed with another instance, false if no other instance is available
    # * raises an exception if a wrong step name is given    
    def replay_macro_step_with_next_instance(current_step, cluster, nodes)
      macro_step = @config.cluster_specific[cluster].get_macro_step(current_step)
      if not macro_step.use_next_instance then
        return false
      else
        decrement_active_threads
        case current_step
        when "SetDeploymentEnv"
          @queue_deployment_environment.push(nodes)
        when "BroadcastEnv"
          @queue_broadcast_environment.push(nodes)
        when "BootNewEnv"
          @queue_boot_new_environment.push(nodes)
        else
          raise "Wrong step name"
        end
        return true
      end
    end

    # Add some nodes in a bad NodeSet
    #
    # Arguments
    # * nodes: NodeSet that must be added in the bad node set
    # Output
    # * nothing
    def add_to_bad_nodes_set(nodes)
      @nodes_ko.add(nodes)
      if one_last_active_thread? then
        #We add an empty node_set to the last state queue
        @queue_process_finished_nodes.push(Nodes::NodeSet.new)
      end
    end

    # Get a new task in the given queue
    #
    # Arguments
    # * queue: name of the queue in which a new task must be taken (SetDeploymentEnv, BroadcastEnv, BootNewEnv, ProcessFinishedNodes)
    # Output
    # * raises an exception if a wrong queue name is given
    def get_task(queue)
      case queue
      when "SetDeploymentEnv"
        return @queue_deployment_environment.pop
      when "BroadcastEnv"
        return @queue_broadcast_environment.pop
      when "BootNewEnv"
        return @queue_boot_new_environment.pop
      when "ProcessFinishedNodes"
        return @queue_process_finished_nodes.pop
      else
        raise "Wrong queue name"
      end
    end

    # Send an exit signal in order to ask the terminaison of the threads (used to avoid deadlock)
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def send_exit_signal
      @queue_deployment_environment.push(MagicCookie.new)
      @queue_broadcast_environment.push(MagicCookie.new)
      @queue_boot_new_environment.push(MagicCookie.new) 
      @queue_process_finished_nodes.push(MagicCookie.new)
    end
  end

  class WorkflowManager
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
    @reboot_window = nil
    @logger = nil
    attr_accessor :nodes_ok
    attr_accessor :nodes_ko

    # Constructor of WorkflowManager
    #
    # Arguments
    # * config: instance of Config
    # * client: Drb handler of the client
    # Output
    # * nothing
    def initialize(config, client, reboot_window)
      @config = config
      @client = client
      if (@config.exec_specific.debug_level != nil) then
        @output = Debug::OutputControl.new(@config.exec_specific.debug_level, client)
      else
        @output = Debug::OutputControl.new(@config.common.debug_level, client)
      end
      @nodes_ok = Nodes::NodeSet.new
      @nodes_ko = Nodes::NodeSet.new
      @nodeset = @config.exec_specific.node_list
      @queue_manager = QueueManager.new(@config, @nodes_ok, @nodes_ko)
      @reboot_window = reboot_window
      @logger = Debug::Logger.new(@nodeset, @config)
      @logger.set("start", Time.now)
      @logger.set("env", @config.exec_specific.environment.name + ":" + @config.exec_specific.environment.version)
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

    # Launch a thread for a macro step
    #
    # Arguments
    # * kind: specifies the kind of macro step to launch
    # Output
    # * nothing  
    def launch_thread_for_macro_step(kind)
      close_thread = false
      @output.debugl(4, "#{kind} thread launched")
      while (not close_thread) do
        nodes = @queue_manager.get_task(kind)
        #We receive the signal to exit
        if (nodes.kind_of?(MagicCookie)) then
          close_thread = true
        else
          if kind != "ProcessFinishedNodes" then
            nodes.group_by_cluster.each_pair { |cluster, set|
              macro_step_instance = @config.cluster_specific[cluster].get_macro_step(kind).get_instance
              instance_name = macro_step_instance[0]
              instance_max_retries = macro_step_instance[1]
              instance_timeout = macro_step_instance[2]
              case kind
              when "SetDeploymentEnv"
                SetDeploymentEnvironnment::SetDeploymentEnvFactory.create(instance_name, 
                                                                          instance_max_retries,
                                                                          instance_timeout,
                                                                          cluster,
                                                                          set,
                                                                          @queue_manager,
                                                                          @reboot_window,
                                                                          @output,
                                                                          @logger).run
              when "BroadcastEnv"
                BroadcastEnvironment::BroadcastEnvFactory.create(instance_name, 
                                                                 instance_max_retries, 
                                                                 instance_timeout,
                                                                 cluster,
                                                                 set,
                                                                 @queue_manager,
                                                                 @reboot_window,
                                                                 @output,
                                                                 @logger).run
              when "BootNewEnv"
                BootNewEnvironment::BootNewEnvFactory.create(instance_name, 
                                                             instance_max_retries,
                                                             instance_timeout,
                                                             cluster,
                                                             set,
                                                             @queue_manager,
                                                             @reboot_window,
                                                             @output,
                                                             @logger).run
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
              @logger.set("success", true, @nodes_ok)
              @nodes_ok.group_by_cluster.each_pair { |cluster, set|
                @output.debugl(1, "Nodes correctly deployed on cluster #{cluster}")
                @output.debugl(1, set.to_s)
              }
              @logger.set("success", false, @nodes_ko)
              @logger.error(@nodes_ko)
              @nodes_ko.group_by_cluster.each_pair { |cluster, set|
                @output.debugl(1, "Nodes not Correctly deployed on cluster #{cluster}")
                @output.debugl(1, set.to_s(true))
              }
              @client.generate_files(@nodes_ok, @nodes_ko)
              @logger.dump
              @queue_manager.send_exit_signal
              @thread_set_deployment_environment.join
              @thread_broadcast_environment.join
              @thread_boot_new_environment.join
            end
          end
        end
      end
    end

    # Create a local dirname for a given file (usefull after a copy in a cache directory)
    #
    # Arguments
    # * file: name of the file on the client side
    # Output
    # * returns the name of the file in the local cache directory
    def use_local_path_dirname(file)
      return @config.common.kadeploy_cache_dir + "/" + File.basename(file)
    end

    # Grab files from the client side (tarball, ssh public key, ...)
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def grab_user_files
      local_tarball = use_local_path_dirname(@config.exec_specific.environment.tarball_file)
      if (not File.exist?(local_tarball)) || (Digest::MD5.hexdigest(File.read(local_tarball)) != @config.exec_specific.environment.tarball_md5) then
        @output.debugl(4, "Grab the tarball file #{@config.exec_specific.environment.tarball_file}")
        @client.get_file(@config.exec_specific.environment.tarball_file)
      end
      @config.exec_specific.environment.tarball_file = local_tarball #now, we use the cached file
      if @config.exec_specific.key != "" then
        @output.debugl(4, "Grab the key file #{@config.exec_specific.key}")
        @client.get_file(@config.exec_specific.key)
        @config.exec_specific.key = use_local_path_dirname(@config.exec_specific.key)
      end
    end

    # Main of WorkflowManager
    #
    # Arguments
    # * nothing
    # Output
    # * nothing  
    def run
      @output.debugl(1, "Launching Kadeploy ...")
      grab_user_files
      @queue_manager.next_macro_step(nil, @nodeset)
      @thread_process_finished_nodes.join
    end
  end
end
