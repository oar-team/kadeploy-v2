#Kadeploy libs
require 'debug'
require 'nodes'
require 'parallel_ops'
require 'cmdctrl_wrapper'
require 'pxe_ops'
require 'cache'

#Ruby libs
require 'ftools'

module MicroStepsLibrary
  class MicroSteps
    @nodes_ok = nil
    @nodes_ko = nil
    @reboot_window = nil
    @config = nil
    @cluster = nil
    @output = nil

    # Constructor of MicroSteps
    #
    # Arguments
    # * nodes_ok: NodeSet of nodes OK
    # * nodes_ko: NodeSet of nodes KO
    # * reboot_window: WindowManager instance
    # * config: instance of Config
    # * cluster: cluster name of the nodes
    # * output: OutputControl instance
    # Output
    # * nothing
    def initialize(nodes_ok, nodes_ko, reboot_window, config, cluster, output)
      @nodes_ok = nodes_ok
      @nodes_ko = nodes_ko
      @reboot_window = reboot_window
      @config = config
      @cluster = cluster
      @output = output
    end

    private

    # Classify an array of nodes in two arrayes (good ones and bad nodes)
    #
    # Arguments
    # * good_bad_array: array that contains nodes ok and ko ([0] are the good ones and [1] are the bad ones)
    # Output
    # * nothing
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
 
    # Wrap a parallel command
    #
    # Arguments
    # * cmd: command to execute on nodes_ok
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * nothing
    def parallel_exec_command_wrapper(cmd, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute(cmd))
    end

    # Wrap a parallel command and expects a given set of exit status
    #
    # Arguments
    # * cmd: command to execute on nodes_ok
    # * status: array of exit status expected
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * nothing
    def parallel_exec_command_wrapper_expecting_status(cmd, status, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute_expecting_status(cmd, status))
    end 
  
    # Wrap a parallel command and expects a given set of exit status and an output
    #
    # Arguments
    # * cmd: command to execute on nodes_ok
    # * status: array of exit status expected
    # * output: string that contains the output expected
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * nothing
    def parallel_exec_command_wrapper_expecting_status_and_output(cmd, status, output, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute_expecting_status_and_output(cmd, status, output))
    end

    # Wrap a parallel send of file
    #
    # Arguments
    # * file: file to send
    # * dest_dir: destination of the file on the nodes
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * nothing
    def parallel_send_file_command_wrapper(file, dest_dir, scattering_kind, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.send_file(file, dest_dir, scattering_kind))
    end

    # Wrap a parallel command that uses an input file
    #
    # Arguments
    # * file: file to send as input
    # * cmd: command to execute on nodes_ok
    # * taktuk_connector: specifies the connector to use with Taktuk
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # * taktuk_connector: specifies the connector to use with Taktuk
    # * status: array of exit status expected
    # Output
    # * nothing
    def parallel_exec_cmd_with_input_file_wrapper(file, cmd, scattering_kind, taktuk_connector, status)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.exec_cmd_with_input_file(file, cmd, scattering_kind, status))
    end

    # Wrap a parallel reboot command and wait a give time the effective reboot
    #
    # Arguments
    # * timeout: time to wait
    # * port: port probed on the rebooted nodes to test
    # Output
    # * nothing    
    def parallel_wait_nodes_after_reboot_wrapper(timeout, port)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, nil)
      classify_nodes(po.wait_nodes_after_reboot(timeout, port))
    end

    # Sub function for reboot_wrapper
    #
    # Arguments
    # * kind: kind of reboot to perform
    # * node_set: NodeSet that must be rebooted 
    # * use_rsh_for_reboot (opt): specify if rsh must be use for soft reboot
    # Output
    # * nothing
    def _reboot_wrapper(kind, node_set, use_rsh_for_reboot = false)
      @output.debugl(4, "A #{kind} reboot will be performed on the nodes #{node_set.to_s}")
      pr = CmdCtrlWrapper::init
      node_set.set.each { |node|
        case kind
        when "soft"
          if (use_rsh_for_reboot) then
            CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_soft_rsh, node)
          else
            CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_soft_ssh, node)
          end
        when "hard"
          CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_hard, node)
        when "very_hard"
          CmdCtrlWrapper::add_cmd(pr, node.cmd.reboot_very_hard, node)
        end
      }
      CmdCtrlWrapper::run(pr)
      classify_nodes(CmdCtrlWrapper::get_results(pr))
    end

    # Wrap the reboot command
    #
    # Arguments
    # * kind: kind of reboot to perform
    # * use_rsh_for_reboot (opt): specify if rsh must be use for soft reboot
    # Output
    # * nothing    
    def reboot_wrapper(kind, use_rsh_for_reboot = false)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)

      callback = Proc.new { |ns|
        map = Array.new
        map.push("soft")
        map.push("hard")
        map.push("very_hard")
        index = map.index(kind)
        finished = false

        while ((index < map.length) && (not finished))
          _reboot_wrapper(map[index], ns, use_rsh_for_reboot)
          if (not @nodes_ko.empty?) then
            ns.free
            @nodes_ko.duplicate_and_free(ns)
            index = index + 1
          else
            finished = true
          end
        end
      }
      @reboot_window.launch_reboot(node_set, &callback)
    end

    # Extract some file from an archive
    #
    # Arguments
    # * archive: archive name
    # * archive_kind: kind of archive
    # * file_array: array of file to extract from the archive
    # * dest_dir: destination dir for the files extracted
    # Output
    # * returns true if the file are extracted correctly, false otherwise
    def extract_files_from_archive(archive, archive_kind, file_array, dest_dir)
      file_array.each { |f|
        case archive_kind
        when "tgz"
          cmd = "tar -C #{dest_dir} --strip 1 -xzf #{archive} #{f}"          
        when "tbz2"
          cmd = "tar -C #{dest_dir} --strip 1 -xjf #{archive} #{f}"
        else
          raise "The kind #{archive_kind} of archive is not supported"
        end
        if not system(cmd) then
          @output.debugl(0, "The file #{f} cannot be extracted")
          return false
        end
      }
      return true
    end

    
    public

    # Change the PXE configuration
    #
    # Arguments
    # * step: kind of change
    # Output
    # * returns true if the operation has been performed correctly, false otherwise
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
        suffix_in_cache = "--e" + @config.exec_specific.environment.id + "v" + @config.exec_specific.environment.version
        kernel = @config.exec_specific.environment.kernel + suffix_in_cache
        initrd = @config.exec_specific.environment.initrd + suffix_in_cache
        images_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
        res = system("touch #{images_dir}/#{kernel}")
        res = res && system("touch #{images_dir}/#{initrd}")
        res = res && PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,
                                                      kernel,
                                                      initrd,
                                                      @config.exec_specific.environment.part,
                                                      @config.common.tftp_repository,
                                                      @config.common.tftp_images_path,
                                                      @config.common.tftp_cfg)
        Cache::clean_cache(@config.common.tftp_repository + "/" + @config.common.tftp_images_path,
                           @config.common.tftp_images_max_size * 1024 * 1024,
                           6,
                           /^.+--e\d+v\d+$/)
      when "back_to_prod_env"
        res = PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,   
                                               @config.cluster_specific[@cluster].prod_kernel,
                                               @config.cluster_specific[@cluster].prod_initrd,
                                               "",
                                               @config.common.tftp_repository,
                                               @config.common.tftp_images_path,
                                               @config.common.tftp_cfg)
      end
      if (res == false) then
        @output.debugl(0, "The PXE configuration has not been performed correctly: #{kind}")
      end
      return res
    end

    # Perform a reboot on the current set of nodes_ok
    #
    # Arguments
    # * reboot_kind: kind of reboot (soft, hard, very_hard, kexec)
    # * use_rsh_for_reboot (opt): specify if rsh must be used for soft reboot
    # Output
    # * returns true (should be false sometimes :D)
    def reboot(reboot_kind, use_rsh_for_reboot = false)
      case reboot_kind
      when "soft"
        reboot_wrapper("soft", use_rsh_for_reboot)
      when "hard"
        reboot_wrapper("hard")
      when "very_hard"
        reboot_wrapper("very_hard")
      when "kexec"
        kernel = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.kernel}"
        initrd = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.initrd}"
        kernel_params = @config.exec_specific.environment.kernel_params
        root_part = @config.exec_specific.environment.part
        #Warning, this require the /usr/local/bin/kexec_detach script
        parallel_exec_command_wrapper("(/usr/local/bin/kexec_detach #{kernel} #{initrd} #{root_part} #{kernel_params})",
                                      @config.common.taktuk_connector)
      end
      return true
    end

    # Check the state of a set of nodes
    #
    # Arguments
    # * step: step in which the nodes are expected to be
    # Output
    # * returns true (should be false sometimes :D)    
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

    # Load some specific drivers on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D) 
    def load_drivers
      cmd = String.new
      @config.cluster_specific[@cluster].drivers.each_index { |i|
        cmd += "modprobe #{@config.cluster_specific[@cluster].drivers[i]};"
      }
      parallel_exec_command_wrapper(cmd, @config.common.taktuk_connector)
      return true
    end

    # Perform a fdisk on the ndoes
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D) 
    def fdisk(kind)
      case kind
      when "prod_env"
        expected_status = "256" #Strange thing, fdisk can not reload the partition table so it exits with 256
      when "untrusted_env"
        expected_status = "0"
      else
        @output.debugl(0, "Invalid kind of deploy environment: #{kind}")
        return false
      end
      parallel_exec_cmd_with_input_file_wrapper(@config.cluster_specific[@cluster].fdisk_file,
                                                "fdisk #{@config.cluster_specific[@cluster].block_device}",
                                                "tree",
                                                @config.common.taktuk_connector,
                                                expected_status)
      return true
    end

    # Perform the deployment part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D) 
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

    # Format the /tmp part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D)     
    def format_tmp_part
      if (@config.exec_specific.reformat_tmp) then
        tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
        parallel_exec_command_wrapper("mkdir -p /tmp; umount #{tmp_part} 2>/dev/null; mkfs.ext2 #{tmp_part}",
                                      @config.common.taktuk_connector)
      end
      return true
    end

    # Mount the deployment part on the ndoes
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D) 
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

    # Mount the /tmp part on the ndoes
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D) 
    def mount_tmp_part
      if (@config.exec_specific.reformat_tmp) then      
        tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
        parallel_exec_command_wrapper("mount #{tmp_part} /tmp",
                                      @config.common.taktuk_connector)
      end
      return true
    end

    # Send a tarball on the nodes (currently unused)
    #
    # Arguments
    # * scattering_kind:  kind of taktuk scatter (tree, chain)
    # Output
    # * returns true (should be false sometimes :D) 
    def send_tarball(scattering_kind)
      parallel_send_file_command_wrapper(@config.exec_specific.environment.tarball_file,
                                         @config.common.tarball_dest_dir,
                                         scattering_kind,
                                         @config.common.taktuk_connector)
      return true
    end

    # Send a tarball and uncompress it on the nodes
    #
    # Arguments
    # * scattering_kind:  kind of taktuk scatter (tree, chain)
    # Output
    # * returns true if the operation is correctly performed, false 
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

    # Send a tarball and uncompress it on the nodes
    #
    # Arguments
    # * scattering_kind:  kind of taktuk scatter (tree, chain)
    # Output
    # * returns true if the operation is correctly performed, false
    def send_key(scattering_kind)
      if (@config.exec_specific.key != "") then
        cmd = "cat - >>#{@config.common.environment_extraction_dir}/root/.ssh/authorized_keys"
        parallel_exec_cmd_with_input_file_wrapper(@config.exec_specific.key,
                                                  cmd,
                                                  scattering_kind,
                                                  @config.common.taktuk_connector,
                                                  "0")       
      end
      return true
    end

    # Wait some nodes after a reboot
    #
    # Arguments
    # * port: port used to perform a reach test on the nodes
    # Output
    # * returns true (should be false sometimes :D)
    def wait_reboot(port)
      parallel_wait_nodes_after_reboot_wrapper(@config.cluster_specific[@cluster].timeout_reboot, port)
      return true
    end
    
    # Copie the kernel and the initrd into the PXE directory
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the operation is correctly performed, false
    def copy_kernel_initrd_to_pxe
      must_extract = false
      archive = @config.exec_specific.environment.tarball_file
      kernel = "boot/" + @config.exec_specific.environment.kernel
      initrd = "boot/" + @config.exec_specific.environment.initrd
      dest_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
      suffix_in_cache = "--e" + @config.exec_specific.environment.id + "v" + @config.exec_specific.environment.version
      cached_kernel = dest_dir + "/" + @config.exec_specific.environment.kernel + suffix_in_cache
      cached_initrd = dest_dir + "/" + @config.exec_specific.environment.initrd + suffix_in_cache
      if not (File.exist?(cached_kernel) && File.exist?(cached_initrd)) then
        must_extract = true
      else
        #If the archive has been modified, re-extraction required
        if (File.mtime(archive).to_i > File.atime(cached_kernel).to_i) ||
            (File.mtime(archive).to_i > File.atime(cached_initrd).to_i) then
          must_extract = true
        end
      end

      if must_extract then
        res = extract_files_from_archive(archive,
                                         @config.exec_specific.environment.tarball_kind,
                                         [kernel, initrd],
                                         @config.common.kadeploy_cache_dir)
        res = res && File.move(@config.common.kadeploy_cache_dir + "/" + @config.exec_specific.environment.kernel, cached_kernel)
        res = res && File.move(@config.common.kadeploy_cache_dir + "/" + @config.exec_specific.environment.initrd, cached_initrd)
        return res
      else
        return true
      end
    end

    # Dummy method to put all the nodes in the node_ko set
    #
    # Arguments
    # * nothing
    # Output
    # * returns true (should be false sometimes :D)
    def produce_bad_nodes
      @nodes_ok.duplicate_and_free(@nodes_ko)
      return true
    end

    # Test if a timeout is reached
    #
    # Arguments
    # * timeout: timeout
    # * instance_thread: instance of thread that waits for the timeout
    # * step_name: name of the current step
    # Output   
    # * returns true if the timeout is reached, false otherwise
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
