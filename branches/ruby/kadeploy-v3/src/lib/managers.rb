#Kadeploy libs
require 'debug'
require 'nodes'
require 'config'
require 'managers'
require 'cache'
require 'stepdeployenv'
require 'stepbroadcastenv'
require 'stepbootnewenv'

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
    # Launch a windowed function
    #
    # Arguments
    # * node_set: instance of NodeSet
    # * callback: reference on block that takes a NodeSet as argument
    # Output
    # * nothing
    def launch(node_set, &callback)
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
        increment_active_threads
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

    # Check if there are some pending events 
    #
    # Arguments
    # * nothing
    # Output
    # * return true if there is no more pending events
    def empty?
      return @queue_deployment_environment.empty? && @queue_broadcast_environment.empty? && 
        @queue_boot_new_environment.empty? && @queue_process_finished_nodes.empty?
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
    @nodes_check_window = nil
    @logger = nil
    @db = nil
    @deployments_table_lock = nil
    @mutex = nil
    attr_accessor :nodes_ok
    attr_accessor :nodes_ko

    # Constructor of WorkflowManager
    #
    # Arguments
    # * config: instance of Config
    # * client: Drb handler of the client
    # * reboot_window: instance of WindowManager to manage the reboot window
    # * nodes_check_window: instance of WindowManager to manage the check of the nodes
    # * db: database handler
    # * deployments_table_lock: mutex to protect the deployments table
    # * syslog_lock: mutex on Syslog
    # Output
    # * nothing
    def initialize(config, client, reboot_window, nodes_check_window, db, deployments_table_lock, syslog_lock)
      @db = db
      @deployments_table_lock = deployments_table_lock
      @config = config
      @client = client
      deploy_id = Time.now.to_i #we use the timestamp as a deploy_id
      if (@config.exec_specific.debug_level != nil) then
        @output = Debug::OutputControl.new(@config.exec_specific.debug_level, client, 
                                           @config.exec_specific.true_user, deploy_id,
                                           @config.common.dbg_to_syslog, @config.common.dbg_to_syslog_level, syslog_lock)
      else
        @output = Debug::OutputControl.new(@config.common.debug_level, client,
                                           @config.exec_specific.true_user, deploy_id,
                                           @config.common.dbg_to_syslog, @config.common.dbg_to_syslog_level, syslog_lock)
      end
      @nodes_ok = Nodes::NodeSet.new
      @nodes_ko = Nodes::NodeSet.new
      @nodeset = @config.exec_specific.node_list
      @queue_manager = QueueManager.new(@config, @nodes_ok, @nodes_ko)
      @reboot_window = reboot_window
      @nodes_check_window = nodes_check_window
      @mutex = Mutex.new
      @logger = Debug::Logger.new(@nodeset, @config, @db, 
                                  @config.exec_specific.true_user, deploy_id, Time.now, 
                                  @config.exec_specific.environment.name + ":" + @config.exec_specific.environment.version, syslog_lock)
#      @logger.set("user", @config.exec_specific.true_user)
#      @logger.set("deploy_id", deploy_id)
#      @logger.set("start", Time.now)
#      @logger.set("env", @config.exec_specific.environment.name + ":" + @config.exec_specific.environment.version)

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
                                                                          @nodes_check_window,
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
                                                                 @nodes_check_window,
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
                                                             @nodes_check_window,
                                                             @output,
                                                             @logger).run
              else
                raise "Invalid macro step name"
              end
              #let's free the memory after the launch of the threads
              GC.start
            }
          else
            #in this case, all is ok
            if not nodes.empty? then
              @nodes_ok.add(nodes)
            end
            # Only the first instance that reaches the end has to manage the exit
            if @mutex.try_lock then
              while ((not @queue_manager.one_last_active_thread?) || (not @queue_manager.empty?))
                sleep 1
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
                @mutex.unlock
              end
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

    # Grab files from the client side (tarball, ssh public key, user postinstall, files for custom operations)
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def grab_user_files
      local_tarball = use_local_path_dirname(@config.exec_specific.environment.tarball["file"])
      if (not File.exist?(local_tarball)) || (Digest::MD5.hexdigest(File.read(local_tarball)) != @config.exec_specific.environment.tarball["md5"]) then
        @output.debugl(4, "Grab the tarball file #{@config.exec_specific.environment.tarball["file"]}")
        @client.get_file(@config.exec_specific.environment.tarball["file"])
      else
        system("touch -a #{local_tarball}")
      end
      @config.exec_specific.environment.tarball["file"] = local_tarball

      if @config.exec_specific.key != "" then
        @output.debugl(4, "Grab the key file #{@config.exec_specific.key}")
        @client.get_file(@config.exec_specific.key)
        @config.exec_specific.key = use_local_path_dirname(@config.exec_specific.key)
      end

      @config.exec_specific.environment.postinstall.each { |postinstall|
        local_postinstall = use_local_path_dirname(postinstall["file"])
        if (not File.exist?(local_postinstall)) || (Digest::MD5.hexdigest(File.read(local_postinstall)) != postinstall["md5"]) then
          @output.debugl(4, "Grab the postinstall file #{postinstall["file"]}")
          @client.get_file(postinstall["file"])
        else
          system("touch -a #{local_postinstall}")
        end
        postinstall["file"] = local_postinstall
      }

      if (@config.exec_specific.custom_operations != nil) then
        @config.exec_specific.custom_operations.each_key { |macro_step|
          @config.exec_specific.custom_operations[macro_step].each_key { |micro_step|
            @config.exec_specific.custom_operations[macro_step][micro_step].each { |entry|
              if (entry[0] == "send") then
                @output.debugl(4, "Grab the file #{entry[1]} for custom operations")
                @client.get_file(entry[1])
                entry[1] = use_local_path_dirname(entry[1])
              end
            }
          }
        }
      end
      Cache::clean_cache(@config.common.kadeploy_cache_dir, @config.common.kadeploy_cache_size, 12, /./)
    end

    # Main of WorkflowManager
    #
    # Arguments
    # * nothing
    # Output
    # * nothing  
    def run
      @output.debugl(1, "Launching Kadeploy ...")
      @deployments_table_lock.lock
      if (@config.exec_specific.ignore_nodes_deploying) then
        nodes_ok = @nodeset
      else
        res = @nodeset.check_nodes_in_deployment(@db, @config.common.purge_deployment_timer)
        nodes_ok = res[0]
        nodes_ko = res[1]
        @output.debugl(1, "The nodes #{nodes_ko.to_s} are already involved in deployment, let's discard them") if (not nodes_ko.empty?)
      end
      #We backup the set of nodes used in the deployement to be able to update their deployment state at the end of the deployment
      nodes_ok_backup = Nodes::NodeSet.new
      nodes_ok.duplicate(nodes_ok_backup)
      nodes_ok.set_deployment_state("deploying", @config.exec_specific.environment.id, @db)
      @deployments_table_lock.unlock
      if (not nodes_ok.empty?) then
        grab_user_files
        @queue_manager.next_macro_step(nil, nodes_ok)
        @thread_process_finished_nodes.join
        @deployments_table_lock.lock
        nodes_ok_backup.set_deployment_state("deployed", nil, @db)
        @deployments_table_lock.unlock
      else
        @output.debugl(1, "All the nodes have been discarded ...")
      end
    end
  end
end
