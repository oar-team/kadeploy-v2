require 'lib/debug'
require 'lib/nodes'
require 'thread'

module Managers
  class MagicCookie
  end

  class WindowManager
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

    # Increments the number of active threads
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

    # Decrements the number of active threads
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

    # Tests if the there is only one active thread
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

    # Goes to the next macro step in the automata
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

    # Replays a step with another instance
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

    # Gets a new task in the given queue
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

    # Sends an exit signal in order to ask the terminaison of the threads (used to avoid deadlock)
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
end
