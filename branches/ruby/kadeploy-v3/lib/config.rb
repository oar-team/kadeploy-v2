require "lib/environment"
require "lib/nodes"

module ConfigInformation
  CONFIGURATION_FOLDER = Dir.pwd + "/conf" #"/etc/kadeploy"
  COMMANDS_FILE = "cmd"
  COMMON_CONFIGURATION_FILE = "conf"
  SPECIFIC_CONFIGURATION_FILE_PREFIX = "specific_conf_"

  class Config
    attr_accessor :common
    attr_accessor :cluster_specific
    
    def initialize
      @common = CommonConfig.new
      @cluster_specific = Hash.new
      add_cluster_specific_config("paravent")
      add_cluster_specific_config("paraci")
    end

    def add_cluster_specific_config(cluster_name)
      @cluster_specific[cluster_name] = ClusterSpecificConfig.new
    end

    def read_nodes(f)
      begin
        return IO::read(f).split("\n").sort.uniq
      rescue
        return []
      end
    end
    
    def bad_option_message(msg)
      puts msg
      puts "Use --help option for correct use"
      return false
    end

    def check_options
      if @common.node_list.empty? then
        return bad_option_message("No nodes list found")
      end
      return true
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
            puts "The #{@common.tftp_repository}/##{@common.tftp_cfg} directory does not exist"
            return false
          end
        end
      end
      return check_options
    end

    def load_cmdline_options
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
        opts.on("-m", "--machine MACHINE", "Node to run on") { |node|
          @common.node_list.push(node)
        }
        opts.on("-f", "--file MACHINELIST", "Files containing list of nodes")  { |f|
          read_nodes(f).each { |node|
            @common.node_list.push(node)
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
    attr_accessor :node_list
    attr_accessor :environment
    attr_accessor :commands       #Hashtable of NodeCmd
    attr_accessor :taktuk_connector
    attr_accessor :taktuk_tree_arity
    attr_accessor :tarball_dest_dir
    
    def initialize
      @debug_level = 3
      @node_list = Array.new
      @rights_kind = "dummy"
      @environment = EnvironmentManagement::Environment.new
      @commands = Hash.new
      init_config
      load_commands
    end

    def init_config
      @tftp_repository = Dir.pwd + "/test/pxe"
      @tftp_images_path = "images"
      @tftp_cfg = "pxelinux.cfg"
      @taktuk_connector = "ssh -o StrictHostKeyChecking=no -o BatchMode=yes"
      @taktuk_tree_arity = 1
      @tarball_dest_dir = "/tmp"
    end

    def load_commands
      commands_file = CONFIGURATION_FOLDER + "/" + COMMANDS_FILE
      if File.exist?(commands_file) then
        IO::read(commands_file).split("\n").each { |line|
          if /(.+)\|(.+)\|(.+)/ =~ line then
            content = Regexp.last_match
            if not @commands.has_key?(content[1])
            then
              @commands[content[1]] = Nodes::NodeCmd.new
            end
            case content[2]
            when "reboot_soft"
              @commands[content[1]].reboot_soft = content[3]
            when "reboot_hard"
              @commands[content[1]].reboot_hard = content[3]
            when "reboot_veryhard"
              @commands[content[1]].reboot_veryhard = content[3]
            when "console"
              @commands[content[1]].console = content[3]
            else
              puts "Unknown command: #{content[2]}"
            end
          end
        }
      else
        raise "Cannot find the command file"
      end
    end
  end
  
  class ClusterSpecificConfig
    attr_accessor :deploy_kernel
    attr_accessor :deploy_initrd
    attr_accessor :block_device
    attr_accessor :deploy_parts     #Array of String, the first one is used by default
    attr_accessor :prod_part
    attr_accessor :workflow_steps   #Array of MacroStep


    
    def initialize
      init_automata
      @deploy_kernel = "deploy-linux-image-2.6.99"
      @deploy_initrd = "deploy-linux-initrd-2.6.99.img ETH_DRV=e1000 ETH_DEV=eth0 DISK_DRV=ahci console=tty0 console=ttyS1,38400n8 ramdisk_size=400000"
      @block_device = "/dev/sda"
      @deploy_parts = Array.new
      @deploy_parts.push("2")
      @prod_part = "1"
    end

    def init_automata
      #init example for the automata, this should be sourced from config file
      @workflow_steps = Array.new
      @workflow_steps.push(MacroStep.new("SetDeploymentEnv",[["SetDeploymentEnvProd",2]]))
      @workflow_steps.push(MacroStep.new("BroadcastEnv",[["BroadcastEnvChainWithFS",2]]))
      @workflow_steps.push(MacroStep.new("BootNewEnv", [["BootNewEnvKexec",1], ["BootNewEnvClassical",2]]))
    end

    def get_macro_step(name)
      @workflow_steps.each { |elt| return elt if (elt.name == name) }
    end
  end

  class MacroStep
    attr_accessor :name
    @array_of_instances = nil #specify the instances by order of use, if the first one fails, we use the second, and so on
    @current = nil

    def initialize(name,array_of_instances)
      @name = name
      @array_of_instances = array_of_instances
      @current = 0
    end

    def use_next_instance
      if (@array_of_instances.length > (@current +1)) then
        @current += 1
      end
    end

    #return an array: [0] is the name of the instance, [1] is the number of retries available for the instance
    def get_instance
      return @array_of_instances[@current]
    end
  end
end
