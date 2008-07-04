require "lib/environment"
require "lib/nodes"
require "optparse"

module ConfigInformation
  CONFIGURATION_FOLDER = Dir.pwd + "/conf" #"/etc/kadeploy"
  COMMANDS_FILE = "cmd"
  NODES_FILE = "nodes"
  COMMON_CONFIGURATION_FILE = "conf"
  SPECIFIC_CONFIGURATION_FILE_PREFIX = "specific_conf_"

  class Config
    attr_accessor :common
    attr_accessor :cluster_specific
    
    def initialize
      @common = CommonConfig.new
      load_common_config_file
      @cluster_specific = Hash.new
      load_specific_config_files
    end

    def load_common_config_file
      common_config_file = CONFIGURATION_FOLDER + "/" + COMMON_CONFIGURATION_FILE
      if File.exist?(common_config_file) then
        IO.readlines(common_config_file).each { |line|
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
              when "taktuk_connector"
                @common.taktuk_connector = content[2]
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
              end
            end
          end
        }
      else
        raise "Cannot find the common configuration file"
      end
    end

    def load_specific_config_files
      Dir[CONFIGURATION_FOLDER + "/" + SPECIFIC_CONFIGURATION_FILE_PREFIX + "*"].each { |f|
        cluster = String.new(f).sub(CONFIGURATION_FOLDER + "/" + SPECIFIC_CONFIGURATION_FILE_PREFIX, "")
        @cluster_specific[cluster] = ClusterSpecificConfig.new
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
              when "deploy_parts"
                content[2].split(",").each { |part|
                  @cluster_specific[cluster].deploy_parts.push(part)
                }
              when "prod_part"
                @cluster_specific[cluster].prod_part = content[2]
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
              when "macrostep"
                macrostep_name = content[2].split("|")[0]
                microstep_list = content[2].split("|")[1]
                tmp = Array.new
                microstep_list.split(",").each { |instance_infos|
                  instance_name = instance_infos.split(":")[0]
                  instance_max_retries = instance_infos.split(":")[1].to_i
                  tmp.push([instance_name, instance_max_retries])
                }
                @cluster_specific[cluster].workflow_steps.push(MacroStep.new(macrostep_name, tmp))
              end
            end
          end
        }
      }
    end

    def read_nodes(f)
      begin
        return IO.readlines(f).sort.uniq
      rescue
        return []
      end
    end
    
    def bad_option_message(msg)
      puts msg
      puts "Use --help option for correct use or --version option to get the version"
      return false
    end

    #Allow to load some specific commands for specific nodes that override generic commands
    def load_commands
      commands_file = CONFIGURATION_FOLDER + "/" + COMMANDS_FILE
      if File.exist?(commands_file) then
        IO.readlines(commands_file).each { |line|
          if not (/^#/ =~ line) then #we ignore commented lines
            if /(.+)\|(.+)\|(.+)/ =~ line then
              content = Regexp.last_match
              node = @common.node_list.get_node_by_host(content[1])
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
      else
        raise "Cannot find the command file"
      end
    end

    def check_options
      if @common.node_list.empty? then
        return bad_option_message("No nodes list found")
      end
      load_commands
      return true
    end
    
    def load_nodes_config_file(f)
      IO.readlines(f).each { |line|
        if /(.*)\ (.*)\ (.*)/ =~ line
          content = Regexp.last_match
          @common.nodes_desc.push(Nodes::Node.new(content[1], content[2], content[3], nil))
        end
      }
    end

    def check_config
      if not File.exist?(@common.tftp_repository) then
        puts "The #{@common.tftp_repository} directory does not exist"
        return false
      else
        if not File.exist?(@common.tftp_repository + "/" + @common.tftp_images_path) then
          puts "The #{@common.tftp_repository}/#{@common.tftp_images_path} directory does not exist"
          return false
        else
          if not File.exist?(@common.tftp_repository + "/" + @common.tftp_cfg) then
            puts "The #{@common.tftp_repository}/#{@common.tftp_cfg} directory does not exist"
            return false
          end
        end
      end

      return check_options
    end
    
    #used to replace several occurence
    def replace_hostname(str, hostname)
      cmd_to_expand = str.clone # we must use this temporary variable since sub() modify the strings
      save = str
      while cmd_to_expand.sub!("HOSTNAME", hostname) != nil  do
        save = cmd_to_expand
      end
      return save
    end

    def generate_commands(hostname, cluster)
      cmd = Nodes::NodeCmd.new
      cmd.reboot_soft = replace_hostname(@cluster_specific[cluster].cmd_soft_reboot, hostname)
      cmd.reboot_hard = replace_hostname(@cluster_specific[cluster].cmd_hard_reboot, hostname)
      cmd.reboot_very_hard = replace_hostname(@cluster_specific[cluster].cmd_very_hard_reboot, hostname)
      cmd.console = replace_hostname(@cluster_specific[cluster].cmd_console, hostname)
      return cmd
    end

    def add_to_node_list(hostname)
      n = @common.nodes_desc.get_node_by_host(hostname)
      if (n != nil) then
        new_node = Nodes::Node.new(n.hostname, n.ip, n.cluster, generate_commands(n.hostname, n.cluster))
        @common.node_list.push(new_node)
      end
    end

    def load_cmdline_options
      if not File.exist?(CONFIGURATION_FOLDER + "/" + NODES_FILE) then
        puts "The #{CONFIGURATION_FOLDER + "/" + NODES_FILE} file does not exist"
        return false
      else
        load_nodes_config_file(CONFIGURATION_FOLDER + "/" + NODES_FILE)
      end

      progname = File::basename($PROGRAM_NAME)
      opts = OptionParser::new do |opts|
        opts.version = "$Id$"
        opts.release = nil
        opts.summary_indent = "  "
        opts.summary_width = 28
        opts.program_name = progname
        opts.banner = "Usage: #{progname} [options]"
        opts.separator "Contact: Emmanuel Jeanvoine <emmanuel.jeanvoine@inria.fr>"
        opts.separator ""
        opts.separator "General options:"
        opts.on("-m", "--machine MACHINE", "Node to run on") { |hostname|
          add_to_node_list(hostname)
        }
        opts.on("-f", "--file MACHINELIST", "Files containing list of nodes")  { |f|
          read_nodes(f).each { |node|
            add_to_node_list(hostname)
          }
        }
        opts.on("-a", "--env-file ENVFILE", "File containing the envrionement description") { |f|
          @common.environment.load_from_file(f)
        }
      end

      opts.parse!(ARGV)
      return check_config
    end
  end
  
  class CommonConfig
    attr_accessor :debug_level
    attr_accessor :tftp_repository
    attr_accessor :tftp_images_path
    attr_accessor :tftp_cfg
    attr_accessor :deploy_db_host
    attr_accessor :deploy_db_name
    attr_accessor :deploy_db_login
    attr_accessor :deploy_db_passwd
    attr_accessor :rights_kind
    attr_accessor :node_list      #list of nodes involved in the deployment
    attr_accessor :nodes_desc     #information about all the nodes
    attr_accessor :environment
    attr_accessor :taktuk_connector
    attr_accessor :taktuk_tree_arity
    attr_accessor :taktuk_auto_propagate
    attr_accessor :tarball_dest_dir
    
    def initialize
      @environment = EnvironmentManagement::Environment.new
      @node_list = Nodes::NodeSet.new
      @nodes_desc = Nodes::NodeSet.new
    end
  end
  
  class ClusterSpecificConfig
    attr_accessor :deploy_kernel
    attr_accessor :deploy_initrd
    attr_accessor :block_device
    attr_accessor :deploy_parts     #Array of String, the first one is used by default
    attr_accessor :prod_part
    attr_accessor :workflow_steps   #Array of MacroStep
    attr_accessor :timeout_reboot
    attr_accessor :cmd_soft_reboot
    attr_accessor :cmd_hard_reboot
    attr_accessor :cmd_very_hard_reboot
    attr_accessor :cmd_console
    
    def initialize
      @deploy_parts = Array.new
      @workflow_steps = Array.new
    end

    def get_macro_step(name)
      @workflow_steps.each { |elt| return elt if (elt.name == name) }
    end
  end

  class MacroStep
    attr_accessor :name
    @array_of_instances = nil #specify the instances by order of use, if the first one fails, we use the second, and so on
    @current = nil

    def initialize(name, array_of_instances)
      @name = name
      @array_of_instances = array_of_instances
      @current = 0
    end

    def use_next_instance
      if (@array_of_instances.length > (@current +1)) then
        @current += 1
        return true
      else
        return false
      end
    end

    #return an array: [0] is the name of the instance, [1] is the number of retries available for the instance
    def get_instance
      return @array_of_instances[@current]
    end
  end
end
