require 'lib/debug'
require 'lib/nodes'
require 'lib/parallel_ops'
require 'lib/cmdctrl_wrapper'
require 'lib/pxe_ops'

module MicroStepsLibrary
  class MicroSteps
    @nodes_ok = nil
    @nodes_ko = nil
    @window_manager = nil
    @config = nil
    @cluster = nil
    @output = nil

    def initialize(nodes_ok, nodes_ko, window_manager, config, cluster, output)
      @nodes_ok = nodes_ok
      @nodes_ko = nodes_ko
      @window_manager = window_manager
      @config = config
      @cluster = cluster
      @output = output
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

    def parallel_send_file_command_wrapper(file, dest_dir, kind)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config)
      classify_nodes(po.send_file(file, dest_dir, kind))
    end

    def parallel_wait_nodes_after_reboot_wrapper(timeout)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config)
      classify_nodes(po.wait_nodes_after_reboot(timeout))
    end

    def reboot_wrapper(kind)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      pr = CmdCtrlWrapper::init
      node_set.set.each { |node|
        CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_soft + "&", node) #run detached
      }
      CmdCtrlWrapper::run(pr)
      classify_nodes(CmdCtrlWrapper::get_results(pr))
    end

    def extract_files_from_archive(archive, file_array, dest_dir)
      file_array.each { |f|
        cmd = "tar -C #{dest_dir} --strip 1 -xzf #{archive} #{f}"
        if not system(cmd) then
          @output.debugl(0, "The file #{f} cannot be extracted")
          return false
        end
      }
      return true
    end

    public

    def switch_pxe(kind)
      case kind
      when "prod_to_deploy_env"
        if (@config.exec_specific.deploy_part != "") then
          deploy_part = @config.exec_specific.deploy_part
        else
          deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part
        end
        res = PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,   
                                               @config.cluster_specific[@cluster].deploy_kernel,
                                               @config.cluster_specific[@cluster].deploy_initrd,
                                               deploy_part,
                                               @config.common.tftp_repository,
                                               @config.common.tftp_images_path,
                                               @config.common.tftp_cfg)
      when "deploy_to_deployed_env"
        res = PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,
                                               @config.exec_specific.environment.kernel,
                                               @config.exec_specific.environment.initrd,
                                               @config.exec_specific.environment.part,
                                               @config.common.tftp_repository,
                                               @config.common.tftp_images_path,
                                               @config.common.tftp_cfg)
      end
      if (res == false) then
        @output.debugl(0, "The PXE configuration has not been performed correctly: #{kind}")
      end
      return res
    end
    
    def reboot(kind)
      case kind
      when "soft"
        reboot_wrapper("soft")
      when "hard"
      when "veryhard"
      when "kexec"
        kernel = "/mnt/dest/boot/#{@config.exec_specific.environment.kernel}"
        initrd = "/mnt/dest/boot/#{@config.exec_specific.environment.initrd}"
        kernel_params = "/mnt/dest/boot/#{@config.exec_specific.environment.kernel_params}"
        root_part = @config.exec_specific.environment.part
        #Warning, this require the /usr/local/bin/kexec_detach script
        parallel_exec_command_wrapper("(/usr/local/bin/kexec_detach #{kernel} #{initrd} #{root_part} #{kernel_params})")
      end
      return true
    end
    
    def check_nodes(kind)
      case kind
      when "deploy_env_booted"
      when "deployed_env_booted"
        #we look if the / mounted partition is the default production partition
        parallel_exec_command_wrapper_expecting_result("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )", 
                                                       @config.exec_specific.environment.part)        
      when "prod_env_booted"
        #we look if the / mounted partition is the default production partition
        parallel_exec_command_wrapper_expecting_result("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )", 
                                                       @config.cluster_specific[@cluster].block_device + \
                                                       @config.cluster_specific[@cluster].prod_part)
      end
      return true
    end

    def load_drivers
      parallel_exec_command_wrapper("date")
      return true
    end

    def fdisk
      parallel_exec_command_wrapper("date")
      return true
    end

    def format_deploy_part
      if (@config.exec_specific.deploy_part != "") then
        deploy_part = @config.exec_specific.deploy_part
      else
        deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part
      end
      parallel_exec_command_wrapper("mkfs.ext3 #{deploy_part}")
      return true
    end
    
    def mount_deploy_part
      if (@config.exec_specific.deploy_part != "") then
        deploy_part = @config.exec_specific.deploy_part
      else
        deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part
      end
      parallel_exec_command_wrapper("(mkdir -p /mnt/dest ; umount /mnt/dest 2>/dev/null ; mount #{deploy_part} /mnt/dest)")
      return true
    end

    def send_tarball(kind)
      parallel_send_file_command_wrapper(@config.exec_specific.environment.tarball_file,
                                         @config.common.tarball_dest_dir,
                                         kind)
      return true
    end

    def uncompress_tarball
      tarball_path = @config.common.tarball_dest_dir + "/" + File.basename(@config.exec_specific.environment.tarball_file)
      parallel_exec_command_wrapper("tar xzvf #{tarball_path} -C /mnt/dest") # ; umount /mnt/dest")
      return true
    end

    def wait_reboot
      parallel_wait_nodes_after_reboot_wrapper(@config.cluster_specific[@cluster].timeout_reboot)
      return true
    end
    
    def copy_kernel_initrd_to_pxe
      archive = @config.exec_specific.environment.tarball_file
      kernel = "boot/" + @config.exec_specific.environment.kernel
      initrd = "boot/" + @config.exec_specific.environment.initrd
      dest_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
      return extract_files_from_archive(archive, [kernel, initrd], dest_dir)
    end

    def produce_bad_nodes
      @nodes_ok.duplicate_and_free(@nodes_ko)
      return true
    end
  end
end
