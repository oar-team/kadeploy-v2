require "lib/environment"
require "lib/nodes"

require "optparse"
require "ostruct"

module ConfigInformation
  CONFIGURATION_FOLDER = Dir.pwd + "/conf" #"/etc/kadeploy"
  COMMANDS_FILE = "cmd"
  NODES_FILE = "nodes"
  COMMON_CONFIGURATION_FILE = "conf"
  CLIENT_CONFIGURATION_FILE = "client_conf"
  SPECIFIC_CONFIGURATION_FILE_PREFIX = "specific_conf_"
  FDISK_FILE_PREFIX = "fdisk_"

  class Config
    public

    attr_reader :common
    attr_reader :cluster_specific
    attr_accessor :exec_specific

    #Constructor of Config
    #
    # Arguments
    # * kind: tool (kadeploy, kaenv, karights, kastat)
    # Output
    # * nothing if all is OK, otherwise raises an exception
    def initialize(kind)
      if (sanity_check(kind) == true) then
        case kind
        when "kadeploy"
          @common = CommonConfig.new
          load_common_config_file
          @cluster_specific = Hash.new
          load_specific_config_files
          load_nodes_config_file
          load_commands
        when "kaenv"
          @common = CommonConfig.new
          load_common_config_file
          load_kaenv_exec_specific
        when "karights"
          @common = CommonConfig.new
          load_common_config_file
          load_karights_exec_specific
        when "kastat"
          @common = CommonConfig.new
          load_common_config_file
          load_kastat_exec_specific    
        else
          raise "Invalid configuration kind: #{kind}"
        end
      else
        puts "Unsane configuration"
        exit(1)
      end
    end

    # Check the config of the Kadeploy tools
    #
    # Arguments
    # * kind: tool (kadeploy, kaenv, karights, kastat)
    # Output
    # * calls the chack_config method that correspond to the selected tool
    def check_config(kind)
      case kind
      when "kadeploy"
        check_kadeploy_config
      when "kaenv"
        check_kaenv_config
      when "karights"
        check_karights_config
      when "kastat"
        check_kastat_config
      end
    end

    # Loads the kadeploy specific stuffs
    #
    # Arguments
    # * nodes_desc: set of nodes read from the configuration file
    # * db: database handler
    # Output
    # * exec_specific: returns an open struct that contains the execution specific information
    #                  or nil if the command line is not correct
    def Config.load_kadeploy_exec_specific(nodes_desc, db)
      exec_specific = OpenStruct.new
      exec_specific.environment = EnvironmentManagement::Environment.new
      exec_specific.node_list = Nodes::NodeSet.new
      exec_specific.load_env_kind = String.new
      exec_specific.load_env_arg = String.new
      exec_specific.env_version = nil #By default we load the latest version
      exec_specific.user = ENV['USER'] #By default, we use the current user
      exec_specific.true_user = ENV['USER']
      exec_specific.deploy_part = String.new
      exec_specific.debug_level = nil
      exec_specific.script = String.new
      exec_specific.key = String.new
      exec_specific.reformat_tmp = false
      exec_specific.steps = Array.new
      if (load_kadeploy_cmdline_options(nodes_desc, exec_specific) == true) then
        case exec_specific.load_env_kind
        when "file"
          if (exec_specific.environment.load_from_file(exec_specific.load_env_arg) == false) then
            return nil
          end
        when "db"
          if (exec_specific.environment.load_from_db(exec_specific.load_env_arg,
                                                     exec_specific.env_version,
                                                     exec_specific.user,
                                                     db) == false) then
            return nil
          end
        else
          raise "Invalid method for environment loading"
        end
        return exec_specific
      else
        return nil
      end
    end

    private

##################################
#         Generic part           #
##################################

    # Performs a test to check the consistancy of the installation
    #
    # Arguments
    # * kind: specifies the program launched (kadeploy|kaenv)
    # Output
    # * returns true if the installation is correct, false otherwise
    def sanity_check(kind)
      #### generic check
      #common configuration file
      if not File.exist?(CONFIGURATION_FOLDER + "/" + COMMON_CONFIGURATION_FILE) then
        puts "The #{CONFIGURATION_FOLDER + "/" + COMMON_CONFIGURATION_FILE} file does not exist"
        return false
      end
      ### command specific check
      case kind
      when "kadeploy"
        #configuration node file
        if not File.exist?(CONFIGURATION_FOLDER + "/" + NODES_FILE) then
          puts "The #{CONFIGURATION_FOLDER + "/" + NODES_FILE} file does not exist"
          return false
        end
      when "kaenv"
      when "karights"
      when "kastat"
      end
      return true
    end

    # Loads the common configuration file
    #
    # Arguments
    # * nothing
    # Output
    # * nothing    
    def load_common_config_file
      IO.readlines(CONFIGURATION_FOLDER + "/" + COMMON_CONFIGURATION_FILE).each { |line|
        if not (/^#/ =~ line) then #we ignore commented lines
          if /(.+)\ \=\ (.+)/ =~ line then
            content = Regexp.last_match
            case content[1]
            when "debug_level"
              @common.debug_level = content[2].to_i
            when "tftp_repository"
              @common.tftp_repository = content[2]
            when "tftp_images_path"
              @common.tftp_images_path = content[2]
            when "tftp_cfg"
              @common.tftp_cfg = content[2]
            when "db_kind"
              @common.db_kind = content[2]
            when "deploy_db_host"
              @common.deploy_db_host = content[2]
            when "deploy_db_name"
              @common.deploy_db_name = content[2]
            when "deploy_db_login"
              @common.deploy_db_login = content[2]
            when "deploy_db_passwd"
              @common.deploy_db_passwd = content[2]
            when "rights_kind"
              @common.rights_kind = content[2]
            when "taktuk_ssh_connector"
              @common.taktuk_ssh_connector = content[2]
            when "taktuk_rsh_connector"
              @common.taktuk_rsh_connector = content[2]
            when "taktuk_tree_arity"
              @common.taktuk_tree_arity = content[2].to_i
            when "taktuk_auto_propagate"
              if content[2] == "true"
                @common.taktuk_auto_propagate = true
              else
                @common.taktuk_auto_propagate = false
              end
            when "tarball_dest_dir"
              @common.tarball_dest_dir = content[2]
            when "kadeploy_server"
              @common.kadeploy_server = content[2]
            when "kadeploy_server_port"
              @common.kadeploy_server_port = content[2].to_i
            when "kadeploy_file_server_port"
              @common.kadeploy_file_server_port = content[2].to_i
            when "kadeploy_tcp_buffer_size"
              @common.kadeploy_tcp_buffer_size = content[2].to_i
            when "kadeploy_cache_dir"
              @common.kadeploy_cache_dir = content[2]
            when "ssh_port"
              @common.ssh_port = content[2]
            when "rsh_port"
              @common.rsh_port = content[2]
            when "environment_extraction_dir"
              @common.environment_extraction_dir = content[2]
            when "log_to_file"
              @common.log_to_file = content[2]
            when "log_to_syslog"
              @common.log_to_syslog = true if (content[2] == "true")
            when "log_to_db"
              @common.log_to_db = true if (content[2] == "true")
            end
          end
        end
      }
    end

    # Loads the client configuration file
    #
    # Arguments
    # * nothing
    # Output
    # * returns an open struct that contains some stuffs usefull for client
    def Config.load_client_config_file
      client_config = OpenStruct.new
      IO.readlines(CONFIGURATION_FOLDER + "/" + CLIENT_CONFIGURATION_FILE).each { |line|
        if not (/^#/ =~ line) then #we ignore commented lines
          if /(.+)\ \=\ (.+)/ =~ line then
            content = Regexp.last_match
            case content[1]
            when "kadeploy_server"
              client_config.kadeploy_server = content[2]
            when "kadeploy_server_port"
              client_config.kadeploy_server_port = content[2].to_i
            end
          end
        end
      }
      return client_config
    end

    # Loads the specific configuration files
    #
    # Arguments
    # * nothing
    # Output
    # * nothing    
    def load_specific_config_files
      Dir[CONFIGURATION_FOLDER + "/" + SPECIFIC_CONFIGURATION_FILE_PREFIX + "*"].each { |f|
        cluster = String.new(f).sub(CONFIGURATION_FOLDER + "/" + SPECIFIC_CONFIGURATION_FILE_PREFIX, "")
        @cluster_specific[cluster] = ClusterSpecificConfig.new
        @cluster_specific[cluster].fdisk_file = CONFIGURATION_FOLDER + "/" + FDISK_FILE_PREFIX + cluster
        IO.readlines(f).each { |line|
          if not (/^#/ =~ line) then #we ignore commented lines
            if /(.+)\ \=\ (.+)/ =~ line then
              content = Regexp.last_match
              case content[1]
              when "deploy_kernel"
                @cluster_specific[cluster].deploy_kernel = content[2]
              when "deploy_initrd"
                @cluster_specific[cluster].deploy_initrd = content[2]
              when "block_device"
                @cluster_specific[cluster].block_device = content[2]
              when "deploy_part"
                @cluster_specific[cluster].deploy_part = content[2]
              when "prod_part"
                @cluster_specific[cluster].prod_part = content[2]
              when "tmp_part"
                @cluster_specific[cluster].tmp_part = content[2]
              when "workflow_steps"
                @cluster_specific[cluster].workflow_steps = content[2]
              when "timeout_reboot"
                @cluster_specific[cluster].timeout_reboot = content[2].to_i
              when "cmd_soft_reboot"
                @cluster_specific[cluster].cmd_soft_reboot = content[2]
              when "cmd_hard_reboot"
                @cluster_specific[cluster].cmd_hard_reboot = content[2]
              when "cmd_very_hard_reboot"
                @cluster_specific[cluster].cmd_very_hard_reboot = content[2]
              when "cmd_console"
                @cluster_specific[cluster].cmd_console = content[2]
              when "drivers"
                content[2].split(",").each { |driver|
                  @cluster_specific[cluster].drivers.push(driver)
                }
              when "macrostep"
                macrostep_name = content[2].split("|")[0]
                microstep_list = content[2].split("|")[1]
                tmp = Array.new
                microstep_list.split(",").each { |instance_infos|
                  instance_name = instance_infos.split(":")[0]
                  instance_max_retries = instance_infos.split(":")[1].to_i
                  instance_timeout = instance_infos.split(":")[2].to_i
                  tmp.push([instance_name, instance_max_retries, instance_timeout])
                }
                @cluster_specific[cluster].workflow_steps.push(MacroStep.new(macrostep_name, tmp))
              end
            end
          end
        }
      }
    end

    # Loads the nodes configuration file
    #
    # Arguments
    # * nothing
    # Output
    # * nothing        
    def load_nodes_config_file
      IO.readlines(CONFIGURATION_FOLDER + "/" + NODES_FILE).each { |line|
        if /(.*)\ (.*)\ (.*)/ =~ line
          content = Regexp.last_match
          host = content[1]
          ip = content[2]
          cluster = content[3]
          @common.nodes_desc.push(Nodes::Node.new(host, ip, cluster, generate_commands(host, cluster)))
        end
      }
    end

    # Eventually loads some specific commands for specific nodes that override generic commands
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def load_commands
      commands_file = CONFIGURATION_FOLDER + "/" + COMMANDS_FILE
      if File.exist?(commands_file) then
        IO.readlines(commands_file).each { |line|
          if not (/^#/ =~ line) then #we ignore commented lines
            if /(.+)\|(.+)\|(.+)/ =~ line then
              content = Regexp.last_match
              node = @common.nodes_desc.get_node_by_host(content[1])
              case content[2]
              when "reboot_soft"
                node.cmd.reboot_soft = content[3]
              when "reboot_hard"
                node.cmd.reboot_hard = content[3]
              when "reboot_veryhard"
              node.cmd.reboot_veryhard = content[3]
              when "console"
                node.cmd.console = content[3]
              else
                puts "Unknown command: #{content[2]}"
              end
            end
          end
        }
      end
    end

    # Replaces the substring HOSTNAME in a string by a value
    #
    # Arguments
    # * str: string in which the HOSTNAME value must be replaced
    # * hostname: value used for the replacement
    # Output
    # * returns the new string       
    def replace_hostname(str, hostname)
      cmd_to_expand = str.clone # we must use this temporary variable since sub() modify the strings
      save = str
      while cmd_to_expand.sub!("HOSTNAME", hostname) != nil  do
        save = cmd_to_expand
      end
      return save
    end

    # Generates the commands used for a node
    #
    # Arguments
    # * hostname: hostname of the node
    # * cluster: cluster whom the node belongs to
    # Output
    # * returns an instance of NodeCmd
    def generate_commands(hostname, cluster)
      cmd = Nodes::NodeCmd.new
      cmd.reboot_soft = replace_hostname(@cluster_specific[cluster].cmd_soft_reboot, hostname)
      cmd.reboot_hard = replace_hostname(@cluster_specific[cluster].cmd_hard_reboot, hostname)
      cmd.reboot_very_hard = replace_hostname(@cluster_specific[cluster].cmd_very_hard_reboot, hostname)
      cmd.console = replace_hostname(@cluster_specific[cluster].cmd_console, hostname)
      return cmd
    end


##################################
#       Kadeploy specific        #
##################################

    # Loads the command-line options of kadeploy
    #
    # Arguments
    # * nodes_desc: set of nodes read from the configuration file
    # * exec_specific: open struct that contains some execution specific stuffs (modified)
    # Output
    # * returns true in case of success, false otherwise
    def Config.load_kadeploy_cmdline_options(nodes_desc, exec_specific)
      progname = File::basename($PROGRAM_NAME)
      opts = OptionParser::new do |opts|
        opts.summary_indent = "  "
        opts.summary_width = 28
        opts.program_name = progname
        opts.banner = "Usage: #{progname} [options]"
        opts.separator "Contact: Emmanuel Jeanvoine <emmanuel.jeanvoine@inria.fr>"
        opts.separator ""
        opts.separator "General options:"
        opts.on("-m", "--machine MACHINE", "Node to run on") { |hostname|
          if not add_to_node_list(hostname, nodes_desc, exec_specific) then
            return false
          end
        }
        opts.on("-f", "--file MACHINELIST", "Files containing list of nodes")  { |f|
          IO.readlines(f).sort.uniq.each { |hostname|
            if not add_to_node_list(hostname.chomp, nodes_desc, exec_specific) then
              return false
            end
          }
        }
        opts.on("-a", "--env-file ENVFILE", "File containing the envrionement description") { |f|
          exec_specific.load_env_kind = "file"
          exec_specific.load_env_arg = f
        }
        opts.on("-e", "--env-name ENVNAME", "Name of the recorded environment to deploy") { |n|
          exec_specific.load_env_kind = "db"
          exec_specific.load_env_arg = n
        }
        opts.on("-v", "--env-version NUMVERSION", "Number of version of the environment to deploy") { |n|
          exec_specific.env_version = n
        }
        opts.on("-u", "--user USERNAME", "Specify the user") { |u|
          exec_specific.user = u
        }
        opts.on("-p", "--partition PARTITION", "Specify the partition to use") { |p|
          exec_specific.deploy_part = p
        }
        opts.on("-d", "--debug-level VALUE", "Debug level between 0 to 4") { |d|
          debug_level = d.to_i
          if ((debug_level > 4) || (debug_level < 0)) then
            puts "Invalid debug level"
            return false
          else
            exec_specific.debug_level = debug_level
          end
        }
        opts.on("-s", "--script FILE", "Execute a script at the end of the deployment") { |f|
          if not File.exist?(f) then
            puts "The file #{f} does not exist"
            return false
          else
            if not File.stat(f).executable? then
              puts "The file #{f} must be executable to be run at the end of the deployment"
              return false
            end
          end
          exec_specific.script = File.expand_path(f)
        }
        opts.on("-k", "--key FILE", "Public key to copy in the root's authorized_keys") { |f|
          if not File.exist?(f) then
            puts "The file #{f} does not exist"
            return false
          end
          exec_specific.key = File.expand_path(f)
        }
        opts.on("-r", "--reformat-tmp", "Reformat the /tmp partition") {
          @exec_specific.reformat_tmp = true
        }
        opts.on("-z", "--force-steps STRING", "Undocumented, for administration purpose only") { |s|
          s.split("&").each { |macrostep|
            macrostep_name = macrostep.split("|")[0]
            microstep_list = macrostep.split("|")[1]
            tmp = Array.new
            microstep_list.split(",").each { |instance_infos|
              instance_name = instance_infos.split(":")[0]
              instance_max_retries = instance_infos.split(":")[1].to_i
              instance_timeout = instance_infos.split(":")[2].to_i
              tmp.push([instance_name, instance_max_retries, instance_timeout])
            }
            exec_specific.steps.push(MacroStep.new(macrostep_name, tmp))
          }
        }
      end
      opts.parse!(ARGV)

      if exec_specific.node_list.empty? then
        puts "Use --help option for correct use"
        return false
      end

      return true
    end

    # Adds a node involved in the deployment to the exec_specific.node_list
    #
    # Arguments
    # * hostname: hostname of the node
    # * nodes_desc: set of nodes read from the configuration file
    # * exec_specific: open struct that contains some execution specific stuffs (modified)
    # Output
    # * returns true if the node exists in the Kadeploy configuration, false otherwise
    def Config.add_to_node_list(hostname, nodes_desc, exec_specific)
      n = nodes_desc.get_node_by_host(hostname)
      if (n != nil) then
        exec_specific.node_list.push(n)
        return true
      else
        puts "The node #{hostname} does not exist in the Kadpeloy configuration"
        return false
      end
    end

    # Checks the whole configuration of the kadeploy execution
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the options used are correct, false otherwise
    # Fixme
    # * should add more tests
    def check_kadeploy_config
      #tftp directory
      if not File.exist?(@common.tftp_repository) then
        puts "The #{@common.tftp_repository} directory does not exist"
        return false
      end
      if not File.exist?(@common.kadeploy_cache_dir) then
        puts "The #{@common.kadeploy_cache_dir} directory does not exist, let's create it"
        res = Dir.mkdir(@common.kadeploy_cache_dir, 0700) rescue false
        if res.kind_of? FalseClass then
          puts "The directory cannot be created"
          return false
        end
      end
      #tftp image directory
      if not File.exist?(@common.tftp_repository + "/" + @common.tftp_images_path) then
        puts "The #{@common.tftp_repository}/#{@common.tftp_images_path} directory does not exist"
        return false
      end
      #tftp config directory
      if not File.exist?(@common.tftp_repository + "/" + @common.tftp_cfg) then
        puts "The #{@common.tftp_repository}/#{@common.tftp_cfg} directory does not exist"
        return false
      end
      return true
    end



##################################
#         Kaenv specific         #
##################################

    # Loads the kaenv specific stuffs
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def load_kaenv_exec_specific
      @exec_specific = OpenStruct.new
      @exec_specific.environment = EnvironmentManagement::Environment.new
      @exec_specific.operation = String.new
      @exec_specific.file = String.new
      @exec_specific.env_name = String.new
      @exec_specific.user = ENV['USER'] #By default, we use the current user
      @exec_specific.show_all_version = false
      load_kaenv_cmdline_options
    end

    # Loads the command-line options of kaenv
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def load_kaenv_cmdline_options
      progname = File::basename($PROGRAM_NAME)
      opts = OptionParser::new do |opts|
        opts.summary_indent = "  "
        opts.summary_width = 28
        opts.program_name = progname
        opts.banner = "Usage: #{progname} [options]"
        opts.separator "Contact: Emmanuel Jeanvoine <emmanuel.jeanvoine@inria.fr>"
        opts.separator ""
        opts.separator "General options:"
        opts.on("-a", "--add ENVFILE", "Add the environment to the environment database") { |f|
          @exec_specific.operation = "add"
          @exec_specific.file = f
        }
        opts.on("-d", "--delete ENVNAME", "Delete the environment from the environment database") { |n|
          @exec_specific.operation = "delete"
          @exec_specific.env_name = n
        }
        opts.on("-l", "--list", "List the environment recorded in the database for a given user") {
          @exec_specific.operation = "list"
        }
        opts.on("-p", "--print ENVNAME", "Print the information about a given environment") { |n|
          @exec_specific.operation = "print"
          @exec_specific.env_name = n
        }
        opts.on("-s", "--show-all-versions", "Show all versions of an environment") {
          @exec_specific.show_all_version = true
        }
        opts.on("-u", "--user USERNAME", "Specify the user") { |u|
          @exec_specific.user = u
        }
      end
      opts.parse!(ARGV)
    end

    # Checks the whole configuration of the kaenv execution
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the options used are correct, false otherwise
    # Fixme
    # * should add more tests
    def check_kaenv_config
      case @exec_specific.operation 
      when "add"
        if not(File.exist?(@exec_specific.file)) then
          puts "The file #{@exec_specific.file} does not exist"
          return false
        end
      when "delete"
      when "list"
      when "print"
      else
        puts "You should choose an operation"
        return false
      end
      return true
    end


##################################
#       Karights specific        #
##################################

    # Loads the karights specific stuffs
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def load_karights_exec_specific
      @exec_specific = OpenStruct.new
      @exec_specific.operation = String.new
      @exec_specific.user = String.new
      @exec_specific.part_list = Array.new
      @exec_specific.node_list = Array.new
      load_karights_cmdline_options
    end

    # Loads the command-line options of karights
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def load_karights_cmdline_options
      progname = File::basename($PROGRAM_NAME)
      opts = OptionParser::new do |opts|
        opts.summary_indent = "  "
        opts.summary_width = 28
        opts.program_name = progname
        opts.banner = "Usage: #{progname} [options]"
        opts.separator "Contact: Emmanuel Jeanvoine <emmanuel.jeanvoine@inria.fr>"
        opts.separator ""
        opts.separator "General options:"
        opts.on("-a", "--add", "Add some rights to a user") {
          @exec_specific.operation = "add"
        }
        opts.on("-d", "--delete", "Delete some rights to a user") {
          @exec_specific.operation = "delete"
        }
        opts.on("-p", "--part PARTNAME", "Include the partition in the operation") { |p|
          @exec_specific.part_list.push(p)
        }
        opts.on("-m", "--machine MACHINE", "Include the machine in the operation") { |m|
          @exec_specific.node_list.push(m)
        }        
        opts.on("-s", "--show-rights", "Show the rights for a given user") {
          @exec_specific.operation = "show"
        }
        opts.on("-u", "--user USERNAME", "Specify the user") { |u|
          @exec_specific.user = u
        }
      end
      opts.parse!(ARGV)
    end

    # Checks the whole configuration of the karigths execution
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the options used are correct, false otherwise
    def check_karights_config
      if (@exec_specific.user == "") then
        puts "You must choose a user"
        return false
      end
      case
      when @exec_specific.operation == "add" || @exec_specific.operation  == "delete"
        if (@exec_specific.part_list.empty?) then
          puts "You must specify at list one partition"
          return false
        end
        if (@exec_specific.node_list.empty?) then
          puts "You must specify at list one node"
          return false
        end
      when @exec_specific.operation == "show"
      else
        puts "You must choose an operation"
        return false
      end
      return true
    end



##################################
#        Kastat specific         #
##################################

    # Loads the kastat specific stuffs
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def load_kastat_exec_specific
      @exec_specific = OpenStruct.new
      @exec_specific.operation = String.new
      @exec_specific.date_min = 0
      @exec_specific.date_max = 0
      @exec_specific.min_retries = 0
      @exec_specific.min_rate = 0
      @exec_specific.node_list = Array.new
      @exec_specific.steps = Array.new
      @exec_specific.fields = Array.new
      load_kastat_cmdline_options
    end

    # Loads the command-line options of kastat
    #
    # Arguments
    # * nothing
    # Output
    # * returns true in case of success, false otherwise
    def load_kastat_cmdline_options
      progname = File::basename($PROGRAM_NAME)
      opts = OptionParser::new do |opts|
        opts.summary_indent = "  "
        opts.summary_width = 28
        opts.program_name = progname
        opts.banner = "Usage: #{progname} [options]"
        opts.separator "Contact: Emmanuel Jeanvoine <emmanuel.jeanvoine@inria.fr>"
        opts.separator ""
        opts.separator "General options:"
        opts.on("-x", "--date-min DATE", "Get the stats from this date (yyyy:mm:dd:hh:mm:ss)") { |d|
          @exec_specific.date_min = d
        }
        opts.on("-y", "--date-max DATE", "Get the stats to this date") { |d|
          @exec_specific.date_max = d
        }
        opts.on("-a", "--list-min-retries NUMBER_OF_RETRIES", "Print the statistics about the nodes that need several attempts") { |n|
          @exec_specific.operation = "list_retries"
          @exec_specific.min_retries = n
        }
        opts.on("-b", "--list-failure-rate", "Print the failure rate for the nodes") { |n|
          @exec_specific.operation = "list_failure_rate"
        }
        opts.on("-c", "--list-min-failure-rate RATE", "Print the nodes which have a minimum failure-rate of RATE (0 <= RATE <= 100") { |r|
          @exec_specific.operation = "list_min_failure_rate"
          @exec_specific.min_rate = r.to_i
        }
        opts.on("-d", "--list-all", "Print all the information") { |r|
          @exec_specific.operation = "list_all"
        }
        opts.on("-s", "--step STEP", "Applies the retry filter on the given steps (1, 2 or 3)") { |s|
          @exec_specific.steps.push(s) 
        }
        opts.on("-m", "--machine MACHINE", "Only print information about the given machines") { |m|
          @exec_specific.node_list.push(m)
        }
        opts.on("-f", "--field FIELD", "Only print the given fields (user,hostname,step1,step2,step3,timeout_step1,timeout_step2,timeout_step3,retry_step1,retry_step2,retry_step3,start,step1_duration,step2_duration,step3_duration,env,md5,success,error)") { |f|
          @exec_specific.fields.push(f)
        }
      end
      opts.parse!(ARGV)
    end

    # Checks the whole configuration of the kastat execution
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the options used are correct, false otherwise
    def check_kastat_config
      authorized_fields = ["user","hostname","step1","step2","step3", \
                           "timeout_step1","timeout_step2","timeout_step3", \
                           "retry_step1","retry_step2","retry_step3", \
                           "start", \
                           "step1_duration","step2_duration","step3_duration", \
                           "env","md5", \
                           "success","error"]
      @exec_specific.fields.each { |f|
        if (not authorized_fields.include?(f)) then
          puts "The field \"#{f}\" does not exist"
          return false
        end
      }
      if (@exec_specific.date_min != 0) then
        if not (/^\d{4}:\d{2}:\d{2}$/ === @exec_specific.date_min) then
          puts "The date #{@exec_specific.date_min} is not correct"
          return false
        else
          str = @exec_specific.date_min.split(":")
          @exec_specific.date_min = Time.mktime(str[0], str[1], str[2], str[3], str[4], str[5]).to_i
        end
      end
      if (@exec_specific.date_max != 0) then
        if not (/^\d{4}:\d{2}:\d{2}$/ === @exec_specific.date_max) then
          puts "The date #{@exec_specific.date_max} is not correct"
          return false
        else
          str = @exec_specific.date_max.split(":")
          @exec_specific.date_max = Time.mktime(str[0], str[1], str[2], str[3], str[4], str[5]).to_i
        end
      end
      authorized_steps = ["1","2","3"]
      @exec_specific.steps.each { |s|
         if (not authorized_steps.include?(s)) then
           puts "The step \"#{s}\" does not exist"
           return false
         end
       }
      return true
    end

  end
  
  class CommonConfig
    attr_accessor :debug_level
    attr_accessor :tftp_repository
    attr_accessor :tftp_images_path
    attr_accessor :tftp_cfg
    attr_accessor :db_kind
    attr_accessor :deploy_db_host
    attr_accessor :deploy_db_name
    attr_accessor :deploy_db_login
    attr_accessor :deploy_db_passwd
    attr_accessor :rights_kind
    attr_accessor :nodes_desc     #information about all the nodes
    attr_accessor :taktuk_ssh_connector
    attr_accessor :taktuk_rsh_connector
    attr_accessor :taktuk_connector
    attr_accessor :taktuk_tree_arity
    attr_accessor :taktuk_auto_propagate
    attr_accessor :tarball_dest_dir
    attr_accessor :kadeploy_server
    attr_accessor :kadeploy_server_port
    attr_accessor :kadeploy_file_server_port
    attr_accessor :kadeploy_tcp_buffer_size
    attr_accessor :kadeploy_cache_dir
    attr_accessor :ssh_port
    attr_accessor :rsh_port
    attr_accessor :environment_extraction_dir
    attr_accessor :log_to_file
    attr_accessor :log_to_syslog
    attr_accessor :log_to_db

    # Constructor of CommonConfig
    #
    # Arguments
    # * nothing
    # Output
    # * nothing    
    def initialize
      @nodes_desc = Nodes::NodeSet.new
    end
  end




  
  class ClusterSpecificConfig
    attr_accessor :deploy_kernel
    attr_accessor :deploy_initrd
    attr_accessor :block_device
    attr_accessor :deploy_part
    attr_accessor :prod_part
    attr_accessor :tmp_part
    attr_accessor :workflow_steps   #Array of MacroStep
    attr_accessor :timeout_reboot
    attr_accessor :cmd_soft_reboot
    attr_accessor :cmd_hard_reboot
    attr_accessor :cmd_very_hard_reboot
    attr_accessor :cmd_console
    attr_accessor :fdisk_file
    attr_accessor :drivers

    # Constructor of ClusterSpecificConfig
    #
    # Arguments
    # * nothing
    # Output
    # * nothing        
    def initialize
      @workflow_steps = Array.new
    end
    
    # Gets the list of the macro step instances associed to a macro step
    #
    # Arguments
    # * name: name of the macro step
    # Output
    # * returns the array of the macro step instances associed to a macro step or nil if the macro step name does not exist
    def get_macro_step(name)
      @workflow_steps.each { |elt| return elt if (elt.name == name) }
      return nil
    end
  end

  class MacroStep
    attr_accessor :name
    @array_of_instances = nil #specify the instances by order of use, if the first one fails, we use the second, and so on
    @current = nil

    # Constructor of MacroStep
    #
    # Arguments
    # * name: name of the macro-step (SetDeploymentEnv, BroadcastEnv, BootNewEnv)
    # * array_of_instances: array of [instance_name, instance_max_retries, instance_timeout]
    # Output
    # * nothing 
    def initialize(name, array_of_instances)
      @name = name
      @array_of_instances = array_of_instances
      @current = 0
    end

    # Selects the next instance implementation for a macro step
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if a next instance exists, false otherwise
    def use_next_instance
      if (@array_of_instances.length > (@current +1)) then
        @current += 1
        return true
      else
        return false
      end
    end

    # Gets the current instance implementation of a macro step
    #
    # Arguments
    # * nothing
    # Output
    # * returns an array: [0] is the name of the instance, 
    #                     [1] is the number of retries available for the instance
    #                     [2] is the timeout for the instance
    def get_instance
      return @array_of_instances[@current]
    end
  end
end
