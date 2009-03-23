#Ruby libs
require 'syslog'

module Debug
  # Print an error message
  #
  # Arguments
  # * msg: error message
  # Output
  # * nothing
  def Debug::client_error(msg)
    puts "ERROR: #{msg}."
    puts "       Use the -h or --help option for correct use."
  end

  class OutputControl
    @debug_level = 0
    @client = nil
    @user = nil
    @deploy_id = nil
    @syslog = nil
    @syslog_dbg_level = nil
    @syslog_lock = nil
    @client_output = nil
    
    # Constructor of OutputControl
    #
    # Arguments
    # * debug_level: debug level at the runtime
    # * client: Drb handler of the client
    # * user: username
    # * deploy_id: id of the deployment
    # * syslog: boolean used to know if syslog must be used or not
    # * syslog_dbg_level: level of debug required in syslog
    # * syslog_lock: mutex on Syslog
    # Output
    # * nothing
    def initialize(debug_level, client, user, deploy_id, syslog, syslog_dbg_level, syslog_lock)
      @debug_level = debug_level
      @client = client
      @user = user
      @deploy_id = deploy_id
      @syslog = syslog
      @syslog_dbg_level = syslog_dbg_level
      @syslog_lock = syslog_lock
      @client_output = true
    end

    def disable_client_output
      @client_output = false
    end

    # Print a message according to a specified debug level
    #
    # Arguments
    # * l: debug level of the message
    # * msg: message
    # Output
    # * prints the message on the server and on the client
    def debugl(l, msg)
      if ((l <= @debug_level) && @client_output)
        @client.print(msg)
      end
      server_str = "#{@deploy_id}|#{@user} -> #{msg}"
      puts server_str
      if (@syslog && (l <= @syslog_dbg_level)) then
        @syslog_lock.lock
        while Syslog.opened?
          sleep 0.2
        end
        sl = Syslog.open("Kadeploy-dbg")
        sl.log(Syslog::LOG_NOTICE, "#{server_str}")
        sl.close
        @syslog_lock.unlock
      end
    end
  end

  class Logger
    @nodes = nil
    @config = nil
    @db = nil
    @syslog_lock = nil

    # Constructor of Logger
    #
    # Arguments
    # * node_set: NodeSet that contains the nodes implied in the deployment
    # * config: instance of Config
    # * db: database handler
    # * user: username
    # * deploy_id: deployment id
    # * start: start time
    # * env: environment name
    # * syslog_lock: mutex on Syslog
    # Output
    # * nothing
    def initialize(node_set, config, db, user, deploy_id, start, env, syslog_lock)
      @nodes = Hash.new
      node_set.make_array_of_hostname.each { |n|
        @nodes[n] = create_node_infos(user, deploy_id, start, env)
      }
      @config = config
      @db = db
      @syslog_lock = syslog_lock
    end

    # Create an hashtable that contains all the information to log
    #
    # Arguments
    # * user: username
    # * deploy_id: deployment id
    # * start: start time
    # * env: environment name
    # Output
    # * returns an Hash instance
    def create_node_infos(user, deploy_id, start, env)
      node_infos = Hash.new
      node_infos["deploy_id"] = deploy_id
      node_infos["user"] = user
      node_infos["step1"] = String.new
      node_infos["step2"] = String.new
      node_infos["step3"] = String.new
      node_infos["timeout_step1"] = 0
      node_infos["timeout_step2"] = 0
      node_infos["timeout_step3"] = 0
      node_infos["retry_step1"] = -1
      node_infos["retry_step2"] = -1
      node_infos["retry_step3"] = -1
      node_infos["start"] = start
      node_infos["step1_duration"] = 0
      node_infos["step2_duration"] = 0
      node_infos["step3_duration"] = 0
      node_infos["env"] = env
      node_infos["md5"] = String.new
      node_infos["success"] = false
      node_infos["error"] = String.new
      return node_infos
    end

    # Set a value for some nodes in the Logger
    #
    # Arguments
    # * op: information to set
    # * val: value for the information
    # * node_set(opt): Array of nodes
    # Output
    # * nothing
    def set(op, val, node_set = nil)
      if (node_set != nil)
        node_set.make_array_of_hostname.each { |n|
          @nodes[n][op] = val
        }
      else
        @nodes.each_key { |k|
          @nodes[k][op] = val
        }
      end
    end

    # Set the error value for a set of nodes
    #
    # Arguments
    # * node_set: Array of nodes
    # Output
    # * nothing      
    def error(node_set)
      node_set.make_array_of_hostname.each { |n|
        @nodes[n]["error"] = node_set.get_node_by_host(n).last_cmd_stderr
      }
    end

    # Increment an information for a set of nodes
    #
    # Arguments
    # * op: information to increment
    # * node_set(opt): Array of nodes
    # Output
    # * nothing 
    def increment(op, node_set = nil)
      if (node_set != nil)
        node_set.make_array_of_hostname.each { |n|
          @nodes[n][op] += 1
        }
      else
        @nodes.each_key { |k|
          @nodes[k][op] += 1
        }
      end
    end

    # Generic method to dump the logged information
    #
    # Arguments
    # * nothing
    # Output
    # * nothing     
    def dump
      dump_to_file if (@config.common.log_to_file != "")
      dump_to_syslog if (@config.common.log_to_syslog)
      dump_to_db if (@config.common.log_to_db)
    end

    # Dump the logged information to syslog
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def dump_to_syslog
      @syslog_lock.lock
      while Syslog.opened?
        sleep 0.2
      end
      sl = Syslog.open("Kadeploy-log")
      @nodes.each_pair { |hostname, node_infos|
        str = node_infos["deploy_id"].to_s + "," + hostname + "," + node_infos["user"]
        str += node_infos["step1"] + "," + node_infos["step2"] + "," + node_infos["step3"]  + ","
        str += node_infos["timeout_step1"].to_s + "," + node_infos["timeout_step2"].to_s + "," + node_infos["timeout_step3"].to_s + ","
        str += node_infos["retry_step1"].to_s + "," + node_infos["retry_step2"].to_s + "," +  node_infos["retry_step3"].to_s + ","
        str += node_infos["start"].to_i.to_s + ","
        str += node_infos["step1_duration"].to_s + "," + node_infos["step2_duration"].to_s + "," + node_infos["step3_duration"].to_s + ","
        str += node_infos["env"] + "," + node_infos["md5"]
        str += node_infos["success"].to_s + "," + node_infos["error"].to_s
        sl.log(Syslog::LOG_NOTICE, "#{str}")
      }
      sl.close
      @syslog_lock.unlock
    end

    # Dump the logged information to the database
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def dump_to_db
      @nodes.each_pair { |hostname, node_infos|
        query = "INSERT INTO log (deploy_id, user, hostname, \
                                  step1, step2, step3, \
                                  timeout_step1, timeout_step2, timeout_step3, \
                                  retry_step1, retry_step2, retry_step3, \
                                  start, \
                                  step1_duration, step2_duration, step3_duration, \
                                  env, md5, \
                                  success, error) \
                        VALUES (\"#{node_infos["deploy_id"]}\", \"#{node_infos["user"]}\", \"#{hostname}\", \
                                \"#{node_infos["step1"]}\", \"#{node_infos["step2"]}\", \"#{node_infos["step3"]}\", \
                                \"#{node_infos["timeout_step1"]}\", \"#{node_infos["timeout_step2"]}\", \"#{node_infos["timeout_step3"]}\", \
                                \"#{node_infos["retry_step1"]}\", \"#{node_infos["retry_step2"]}\", \"#{node_infos["retry_step3"]}\", \
                                \"#{node_infos["start"].to_i}\", \
                                \"#{node_infos["step1_duration"]}\", \"#{node_infos["step2_duration"]}\", \"#{node_infos["step3_duration"]}\", \
                                \"#{node_infos["env"]}\", \"#{node_infos["md5"]}\", \
                                \"#{node_infos["success"]}\", \"#{node_infos["error"]}\")"
        res = @db.run_query(query)
      }
    end

    # Dump the logged information to a file
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def dump_to_file
      fd = File.new(@config.common.log_to_file, File::CREAT | File::APPEND | File::WRONLY, 0644)
      fd.flock(File::LOCK_EX)
      @nodes.each_pair { |hostname, node_infos|
        str = node_infos["deploy_id"].to_s + "," + hostname + "," + node_infos["user"]
        str += node_infos["step1"] + "," + node_infos["step2"] + "," + node_infos["step3"]  + ","
        str += node_infos["timeout_step1"].to_s + "," + node_infos["timeout_step2"].to_s + "," + node_infos["timeout_step3"].to_s + ","
        str += node_infos["retry_step1"].to_s + "," + node_infos["retry_step2"].to_s + "," +  node_infos["retry_step3"].to_s + ","
        str += node_infos["start"].to_i.to_s + ","
        str += node_infos["step1_duration"].to_s + "," + node_infos["step2_duration"].to_s + "," + node_infos["step3_duration"].to_s + ","
        str += node_infos["env"] + "," + node_infos["md5"]
        str += node_infos["success"].to_s + "," + node_infos["error"].to_s
        fd.write("#{Time.now.to_i}: #{str}\n")
      }
      fd.flock(File::LOCK_UN)
      fd.close
    end
  end
end