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

    def parallel_exec_command_wrapper(cmd, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute(cmd))
    end

    def parallel_exec_command_wrapper_expecting_status(cmd, status, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute_expecting_status(cmd, status))
    end    

    def parallel_exec_command_wrapper_expecting_status_and_output(cmd, status, output, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute_expecting_status_and_output(cmd, status, output))
    end

    def parallel_send_file_command_wrapper(file, dest_dir, kind, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.send_file(file, dest_dir, kind))
    end

    def parallel_exec_cmd_with_input_file_wrapper(file, cmd, scattering_kind, taktuk_connector, status)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.exec_cmd_with_input_file(file, cmd, scattering_kind, status))
    end

    def parallel_wait_nodes_after_reboot_wrapper(timeout, port)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, nil)
      classify_nodes(po.wait_nodes_after_reboot(timeout, port))
    end

    def _reboot_wrapper(kind, node_set)
      @output.debugl(4, "A #{kind} reboot will be performed on the nodes #{node_set.to_s}")
      pr = CmdCtrlWrapper::init
      node_set.set.each { |node|
        case kind
        when "soft"
          CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_soft, node)
        when "hard"
          CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_hard, node)
        when "very_hard"
          CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_very_hard, node)
        end
      }
      CmdCtrlWrapper::run(pr)
      classify_nodes(CmdCtrlWrapper::get_results(pr))
    end

    def reboot_wrapper(kind)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      map = Array.new
      map.push("soft")
      map.push("hard")
      map.push("very_hard")
      index = map.index(kind)
      finished = false
      while ((index < map.length) && (not finished))
        _reboot_wrapper(map[index], node_set)
        if (not @nodes_ko.empty?) then
          node_set.free
          @nodes_ko.duplicate_and_free(node_set)
          index = index + 1
        else
          finished = true
        end
      end
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

    def switch_pxe(step)
      case step
      when "prod_to_deploy_env"
        res = PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,   
                                               @config.cluster_specific[@cluster].deploy_kernel,
                                               @config.cluster_specific[@cluster].deploy_initrd,
                                               "",
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
    
    def reboot(reboot_kind)
      case reboot_kind
      when "soft"
        reboot_wrapper("soft")
      when "hard"
        reboot_wrapper("hard")
      when "very_hard"
        reboot_wrapper("very_hard")
      when "kexec"
        kernel = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.kernel}"
        initrd = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.initrd}"
        kernel_params = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.kernel_params}"
        root_part = @config.exec_specific.environment.part
        #Warning, this require the /usr/local/bin/kexec_detach script
        parallel_exec_command_wrapper("(/usr/local/bin/kexec_detach #{kernel} #{initrd} #{root_part} #{kernel_params})",
                                      @config.common.taktuk_connector)
      end
      return true
    end
    
    def check_nodes(step)
      case step
      when "deploy_env_booted"
        parallel_exec_command_wrapper_expecting_status_and_output("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )",
                                                                  ["0"],
                                                                  "/dev/ram1",
                                                                  @config.common.taktuk_connector)
      when "deployed_env_booted"
        #we look if the / mounted partition is the default production partition
        parallel_exec_command_wrapper_expecting_status_and_output("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )",
                                                                  ["0"],
                                                                  @config.exec_specific.environment.part,
                                                                  @config.common.taktuk_connector)
      when "prod_env_booted"
        #we look if the / mounted partition is the default production partition
        parallel_exec_command_wrapper_expecting_status_and_output("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )",
                                                                  ["0"],
                                                                  @config.cluster_specific[@cluster].block_device + \
                                                                  @config.cluster_specific[@cluster].prod_part,
                                                                  @config.common.taktuk_connector)
      end
      return true
    end

    def load_drivers
#      parallel_exec_command_wrapper("date", @config.common.taktuk_connector)
      return true
    end

    def fdisk
      parallel_exec_cmd_with_input_file_wrapper(@config.cluster_specific[@cluster].fdisk_file,
                                                "fdisk #{@config.cluster_specific[@cluster].block_device}",
                                                "tree",
                                                @config.common.taktuk_connector,
                                                "256")  #Strange thing, fdisk can not reload the partition table so it exits with 256
      return true
    end

    def format_deploy_part
      if (@config.exec_specific.deploy_part != "") then
        deploy_part = @config.exec_specific.deploy_part
      else
        deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part
      end

      parallel_exec_command_wrapper("mkdir -p #{@config.common.environment_extraction_dir}; umount #{deploy_part} 2>/dev/null; mkfs.ext2 #{deploy_part}",
                                    @config.common.taktuk_connector)
      return true
    end
    
    def format_tmp_part
      if (@config.exec_specific.reformat_tmp) then
        tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
        parallel_exec_command_wrapper("mkdir -p /tmp; umount #{tmp_part} 2>/dev/null; mkfs.ext2 #{tmp_part}",
                                      @config.common.taktuk_connector)
      end
      return true
    end

    def mount_deploy_part
      if (@config.exec_specific.deploy_part != "") then
        deploy_part = @config.exec_specific.deploy_part
      else
        deploy_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part
      end
      parallel_exec_command_wrapper("mount #{deploy_part} #{@config.common.environment_extraction_dir}",
                                    @config.common.taktuk_connector)
      return true
    end

    def mount_tmp_part
      if (@config.exec_specific.reformat_tmp) then      
        tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
        parallel_exec_command_wrapper("mount #{tmp_part} /tmp",
                                      @config.common.taktuk_connector)
      end
      return true
    end

    def send_tarball(scattering_kind)
      parallel_send_file_command_wrapper(@config.exec_specific.environment.tarball_file,
                                         @config.common.tarball_dest_dir,
                                         scattering_kind,
                                         @config.common.taktuk_connector)
      return true
    end

    def send_tarball_and_uncompress(scattering_kind)
      case @config.exec_specific.environment.tarball_kind
      when "tgz"
        cmd = "tar xz -C #{@config.common.environment_extraction_dir}"
      when "tbz2"
        cmd = "tar xj -C #{@config.common.environment_extraction_dir}"
      when "ddgz"
        cmd = "gzip -cd > #{@config.common.environment_extraction_dir}"
      when "ddbz2"
        cmd = "bzip2 -cd > #{@config.common.environment_extraction_dir}"
      else
        @output.debugl(0, "The #{@config.exec_specific.environment.tarball_kind} archive kind is not supported")
        return false
      end
      parallel_exec_cmd_with_input_file_wrapper(@config.exec_specific.environment.tarball_file,
                                                cmd,
                                                scattering_kind,
                                                @config.common.taktuk_connector,
                                                "0")
      return true
    end

    def send_key(scattering_kind)
      if (@config.exec_specific.key != "") then
        cmd = "cat - >>#{@config.common.environment_extraction_dir}/root/.ssh/authorized_keys"
        parallel_exec_cmd_with_input_file_wrapepr(@config.exec_specific.key,
                                                  cmd,
                                                  scattering_kind,
                                                  @config.common.taktuk_connector,
                                                  "0")       
      end
      return true
    end

    def wait_reboot(port)
      parallel_wait_nodes_after_reboot_wrapper(@config.cluster_specific[@cluster].timeout_reboot, port)
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
    
    #return true if the timeout is reached
    def timeout?(timeout, instance_thread, step_name)
      start = Time.now.to_i
      while ((instance_thread.status != false) && (Time.now.to_i < (start + timeout)))
        sleep 1
      end
      if (instance_thread.status != false) then
        @output.debugl(4, "Timeout before the end of the step, let's kill the instance")
        Thread.kill(instance_thread)
        @nodes_ok.duplicate_and_free(@nodes_ko)
        @nodes_ko.set_error_msg("Timeout in the #{step_name} step")
        return true
      else
        return false
      end
    end
  end
end
