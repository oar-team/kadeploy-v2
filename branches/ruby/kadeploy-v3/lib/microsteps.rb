require 'lib/debug'
require 'lib/nodes'
require 'lib/parallel_ops'
require 'lib/pxe_ops'

module MicroStepsLibrary
  class MicroSteps
    @nodes_ok = nil
    @nodes_ko = nil
    @window_manager = nil
    @config = nil
    @cluster = nil

    def initialize(nodes_ok, nodes_ko, window_manager, config, cluster)
      @nodes_ok = nodes_ok
      @nodes_ko = nodes_ko
      @window_manager = window_manager
      @config = config
      @cluster = cluster
    end

    #good_bad_array[0] are the good ones and good_bad_array[1] are the bad ones
    def classify_nodes(good_bad_array)
      if not good_bad_array[0].empty? then
        good_bad_array[0].each { |n|
          @nodes_ok.push(n)
        }
      end
      if not good_bad_array[1].empty? then
        good_bad_array[1].each { |n|
          @nodes_ko.push(n)
        }
      end
    end

    #good_bad_array[0] are the good ones and good_bad_array[1] are the bad ones
    def classify_nodes_expecting_result(good_bad_array, result)
      if not good_bad_array[0].empty? then
        good_bad_array[0].each { |n|
          if n.last_cmd_stdout.split("\n")[0] == result then #we only grab the first line
            @nodes_ok.push(n)
          else
            @nodes_ko.push(n)
          end
        }
      end
      if not good_bad_array[1].empty? then
        good_bad_array[1].each { |n|
          @nodes_ko.push(n)
        }
      end
    end   

    def parallel_exec_command_wrapper(cmd)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config)
      classify_nodes(po.execute(cmd))
    end

    def parallel_exec_command_wrapper_expecting_result(cmd, result)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config)
      classify_nodes_expecting_result(po.execute(cmd), result)
    end

    def parallel_send_file_command_wrapper(file, dest_dir)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config)
      classify_nodes(po.send_file(file, dest_dir))
    end

    def switch_pxe(kind)
      ops = PXEOperations::PXEOps.new
      case kind
      when "prod_to_deploy_env"
        ops.set_pxe_for_linux(@nodes_ok.make_array_of_ip,
                              @config.cluster_specific[@cluster].deploy_kernel,
                              @config.cluster_specific[@cluster].deploy_initrd,
                              @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_parts[0],
                              @config.common.tftp_repository,
                              @config.common.tftp_images_path,
                              @config.common.tftp_cfg)
      when "deploy_to_deployed_env"
        ops.set_pxe_for_linux(@nodes_ok.make_array_of_ip,
                              @config.common.environment.kernel,
                              @config.common.environment.initrd,
                              @config.common.environment.part,
                              @config.common.tftp_repository,
                              @config.common.tftp_images_path,
                              @config.common.tftp_cfg)
      end
    end
    
    def reboot(kind)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      case kind
      when "soft"
      when "hard"
      when "veryhard"
      end
      node_set.duplicate_and_free(@nodes_ok)
    end
    
    def check_nodes(kind)
      
      case kind
      when "deploy_env_booted"
      when "deployed_env_booted"
      when "prod_env_booted"
        #we look if the / mounted partition is the default production partition
        parallel_exec_command_wrapper_expecting_result("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )", 
                                                       @config.cluster_specific[@cluster].block_device + \
                                                       @config.cluster_specific[@cluster].prod_part)
      end
    end

    def load_drivers
      parallel_exec_command_wrapper("date")
    end

    def fdisk
      parallel_exec_command_wrapper("date")
    end

    def format_deploy_part
      parallel_exec_command_wrapper("date")
    end
    
    def mount_deploy_part
      deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part)
      parallel_exec_command_wrapper("(mkdir -p /mnt/dest ; mount #{deploy_part} /mnt/dest)")
    end

    def send_tarball
      parallel_send_file_command_wrapper(@config.common.environment.tarball_file, @config.common.tarball_dest_dir)
    end

    def uncompress_tarball
      tarball_path = @config.common.tarball_dest_dir + "/" + @config.common.environment.tarball_file
      parallel_send_file_command_wrapper("tar xzvf #{tarball_path} -C /mnt/dest")
    end

  end
end
