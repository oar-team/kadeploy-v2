module ConfigInformation
  CONFIGURATION_FOLDER = "/etc/kadeploy"
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

    def load_cmdline_options
      progname = File::basename($PROGRAM_NAME)

      opts = OptionParser::new do |opts|
        opts.version = "$Id: $"
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
      end

      opts.parse!(ARGV)
      return check_options
    end
  end
  
  class CommonConfig
    attr_accessor :debug_level
    attr_accessor :deploy_db_host
    attr_accessor :deploy_db_name
    attr_accessor :deploy_db_login
    attr_accessor :deploy_db_passwd
    attr_accessor :rights_kind
    attr_accessor :node_list
    attr_accessor :env_file

    def initialize
      @debug_level = 3
      @node_list = Array.new
      @rights_kind = "dummy"
    end
  end
  
  class ClusterSpecificConfig
    attr_accessor :label_grub
    attr_accessor :kernel_param
    attr_accessor :deployment_parts #list, the first one is used by default
    attr_accessor :workflow_steps   #Array of MacroStep
    attr_accessor :default_block_device
    attr_accessor :default_device_number
    
    def initialize
      init_automata
    end

    def init_automata
      #init example for the automata, this should be sourced from config file
      @workflow_steps = Array.new
      @workflow_steps.push(MacroStep.new("SetDeploymentEnv",[["SetDeploymentEnvUntrusted",3]]))
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
