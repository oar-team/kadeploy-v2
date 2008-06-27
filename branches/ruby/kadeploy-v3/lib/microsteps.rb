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

    private

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

    def parallel_wait_nodes_after_reboot_wrapper(timeout)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config)
      classify_nodes(po.wait_nodes_after_reboot(timeout))
    end

    def extract_files_from_archive(archive, file_array, dest_dir)
      file_array.each { |f|
        cmd = "tar -C #{dest_dir} --strip 1 -xzvf #{archive} #{f}"
        if not system(cmd) then
          puts "The file #{f} cannot be extracted"
          return false
        end
      }
      return true
    end

    public

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
      case kind
      when "soft"
      when "hard"
      when "veryhard"
      when "kexec"
        kernel = "/mnt/dest/boot/#{@config.common.environment.kernel}"
        initrd = "/mnt/dest/boot/#{@config.common.environment.initrd}"
        kernel_params = "/mnt/dest/boot/#{@config.common.environment.kernel_params}"
        root_part = @config.common.environment.part
        #Warning, this require the /usr/local/bin/kexec_detach script
        parallel_exec_command_wrapper("(/usr/local/bin/kexec_detach #{kernel} #{initrd} #{root_part} #{kernel_params})")
      end
    end
    
    def check_nodes(kind)
      case kind
      when "deploy_env_booted"
      when "deployed_env_booted"
        #we look if the / mounted partition is the default production partition
        parallel_exec_command_wrapper_expecting_result("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )", 
                                                       @config.common.environment.part)        
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
      deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_parts[0]
      parallel_exec_command_wrapper("mkfs.ext3 #{deploy_part}")
    end
    
    def mount_deploy_part
      deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_parts[0]
      parallel_exec_command_wrapper("(mkdir -p /mnt/dest ; umount /mnt/dest 2>/dev/null ; mount #{deploy_part} /mnt/dest)")
    end

    def send_tarball
      parallel_send_file_command_wrapper(@config.common.environment.tarball_file, @config.common.tarball_dest_dir)
    end

    def uncompress_tarball
      tarball_path = @config.common.tarball_dest_dir + "/" + File.basename(@config.common.environment.tarball_file)
      parallel_exec_command_wrapper("tar xzvf #{tarball_path} -C /mnt/dest") # ; umount /mnt/dest")
    end

    def wait_reboot
      parallel_wait_nodes_after_reboot_wrapper(@config.cluster_specific[@cluster].timeout_reboot)
    end
    
    def copy_kernel_initrd_to_pxe
      archive = @config.common.environment.tarball_file
      kernel = "boot/" + @config.common.environment.kernel
      intird= "boot/" + @config.common.environment.initrd
      dest_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
      extract_files_from_archive(archive, [kernel, initrd], dest_dir)
    end
  end
end
