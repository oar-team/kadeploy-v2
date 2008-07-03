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
      @cluster_specific = Hash.new
      add_cluster_specific_config("g5kdev-cluster")
      add_cluster_specific_config("g5kdev-cluster2")
    end

    def add_cluster_specific_config(cluster_name)
      @cluster_specific[cluster_name] = ClusterSpecificConfig.new
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
    attr_accessor :move_pxe_cfg
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
      @debug_level = 3
      @rights_kind = "dummy"
      @environment = EnvironmentManagement::Environment.new
      @node_list = Nodes::NodeSet.new
      @nodes_desc = Nodes::NodeSet.new
      init_config
    end

    def init_config
#      @tftp_repository = Dir.pwd + "/test/pxe"
      @tftp_repository = "/var/lib/tftpboot"
      @tftp_images_path = "kernels"
      @tftp_cfg = "pxelinux.cfg"
      @taktuk_connector = "ssh -o StrictHostKeyChecking=no -o BatchMode=yes"
      @taktuk_tree_arity = 1
      @tarball_dest_dir = "/tmp"
      @taktuk_auto_propagate = true
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
      init_automata
      @deploy_kernel = "deploy-linux-image-2.6.99"
      @deploy_initrd = "deploy-linux-initrd-2.6.99.img ETH_DRV=e1000 ETH_DEV=eth0 DISK_DRV=ahci console=tty0 console=ttyS1,38400n8 ramdisk_size=400000"
      @block_device = "/dev/hda"
      @deploy_parts = Array.new
      @deploy_parts.push("3")
      @prod_part = "2"
      @timeout_reboot = 120
      @cmd_soft_reboot = "ssh -q -o BatchMode=yes -o StrictHostKeyChecking=no -o ConnectTimeout=2 root@HOSTNAME /sbin/reboot"
      @cmd_hard_reboot = "vmware-cmd /home/ejeanvoi/vmware/HOSTNAME/HOSTNAME.vmx reset hard"
      @cmd_very_hard_reboot = ""
      @cmd_console = ""
    end

    def init_automata
      #init example for the automata, this should be sourced from config file
      @workflow_steps = Array.new
      @workflow_steps.push(MacroStep.new("SetDeploymentEnv",[["SetDeploymentEnvProd",2]]))
      @workflow_steps.push(MacroStep.new("BroadcastEnv",[["BroadcastEnvChainWithFS",2]]))
#      @workflow_steps.push(MacroStep.new("BootNewEnv", [["BootNewEnvKexec",1], ["BootNewEnvClassical",2]]))
      @workflow_steps.push(MacroStep.new("BootNewEnv", [["BootNewEnvClassical",1], ["BootNewEnvClassical",2]]))
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
