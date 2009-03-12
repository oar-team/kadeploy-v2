#Kadeploy libs
require 'debug'
require 'nodes'
require 'parallel_ops'
require 'cmdctrl_wrapper'
require 'pxe_ops'
require 'cache'
require 'bittorrent'

#Ruby libs
require 'ftools'

module MicroStepsLibrary
  class MicroSteps
    attr_accessor :nodes_ok
    attr_accessor :nodes_ko
    @reboot_window = nil
    @config = nil
    @cluster = nil
    @output = nil
    @macro_step = nil

    # Constructor of MicroSteps
    #
    # Arguments
    # * nodes_ok: NodeSet of nodes OK
    # * nodes_ko: NodeSet of nodes KO
    # * reboot_window: WindowManager instance
    # * nodes_check_window: WindowManager instance
    # * config: instance of Config
    # * cluster: cluster name of the nodes
    # * output: OutputControl instance
    # * macro_step: name of the current MacroStep instance
    # Output
    # * nothing
    def initialize(nodes_ok, nodes_ko, reboot_window, nodes_check_window, config, cluster, output, macro_step)
      @nodes_ok = nodes_ok
      @nodes_ko = nodes_ko
      @reboot_window = reboot_window
      @nodes_check_window = nodes_check_window
      @config = config
      @cluster = cluster
      @output = output
      @macro_step = macro_step
    end

    private

    # Classify an array of nodes in two NodeSet (good ones and bad nodes)
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
 
    # Classify an array of nodes in two NodeSet (good ones and bad nodes) but does not modify @nodes_ko
    #
    # Arguments
    # * good_bad_array: array that contains nodes ok and ko ([0] are the good ones and [1] are the bad ones)
    # Output
    # * return a NodeSet of bad nodes or nil if there is no bad nodes
    def classify_only_good_nodes(good_bad_array)
      if not good_bad_array[0].empty? then
        good_bad_array[0].each { |n|
          @nodes_ok.push(n)
        }
      end
      if not good_bad_array[1].empty? then
        bad_nodes = Nodes::NodeSet.new
        good_bad_array[1].each { |n|
          bad_nodes.push(n)
        }
        return bad_nodes
      else
        return nil
      end
    end

    # Wrap a parallel command
    #
    # Arguments
    # * cmd: command to execute on nodes_ok
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * return true if the command has been successfully ran on one node at least, false otherwise
    def parallel_exec_command_wrapper(cmd, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute(cmd))
      return (not @nodes_ok.empty?)
    end

    # Wrap a parallel command and expects a given set of exit status
    #
    # Arguments
    # * cmd: command to execute on nodes_ok
    # * status: array of exit status expected
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * return true if the command has been successfully ran on one node at least, false otherwise
    def parallel_exec_command_wrapper_expecting_status(cmd, status, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute_expecting_status(cmd, status))
      return (not @nodes_ok.empty?)
    end 
  
    # Wrap a parallel command and expects a given set of exit status and an output
    #
    # Arguments
    # * cmd: command to execute on nodes_ok
    # * status: array of exit status expected
    # * output: string that contains the output expected
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * return true if the command has been successfully ran on one node at least, false otherwise
    def parallel_exec_command_wrapper_expecting_status_and_output(cmd, status, output, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.execute_expecting_status_and_output(cmd, status, output))
      return (not @nodes_ok.empty?)
    end

    # Wrap a parallel send of file
    #
    # Arguments
    # * file: file to send
    # * dest_dir: destination of the file on the nodes
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # * taktuk_connector: specifies the connector to use with Taktuk
    # Output
    # * return true if the file has been successfully sent on one node at least, false otherwise
    def parallel_send_file_command_wrapper(file, dest_dir, scattering_kind, taktuk_connector)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.send_file(file, dest_dir, scattering_kind))
      return (not @nodes_ok.empty?)
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
    # * return true if the command has been successfully ran on one node at least, false otherwise
    def parallel_exec_cmd_with_input_file_wrapper(file, cmd, scattering_kind, taktuk_connector, status)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, taktuk_connector)
      classify_nodes(po.exec_cmd_with_input_file(file, cmd, scattering_kind, status))
      return (not @nodes_ok.empty?)
    end

    # Wrap a parallel wait command
    #
    # Arguments
    # * timeout: time to wait
    # * ports_up: up ports probed on the rebooted nodes to test
    # * ports_down: down ports probed on the rebooted nodes to test
    # * nodes_check_window: instance of WindowManager
    # Output
    # * return true if at least one node has been successfully rebooted, false otherwise
    def parallel_wait_nodes_after_reboot_wrapper(timeout, ports_up, ports_down, nodes_check_window)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set, @config, nil)
      classify_nodes(po.wait_nodes_after_reboot(timeout, ports_up, ports_down, nodes_check_window))
      return (not @nodes_ok.empty?)
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
      return classify_only_good_nodes(CmdCtrlWrapper::get_results(pr))
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
        bad_nodes = Nodes::NodeSet.new
        map = Array.new
        map.push("soft")
        map.push("hard")
        map.push("very_hard")
        index = map.index(kind)
        finished = false
        
        while ((index < map.length) && (not finished))
          bad_nodes = _reboot_wrapper(map[index], ns, use_rsh_for_reboot)
          if (bad_nodes != nil) then
            ns.free
            index = index + 1
            if (index < map.length) then
              bad_nodes.duplicate_and_free(ns)
            else
              @nodes_ko.add(bad_nodes)
            end
          else
            finished = true
          end
        end
      }
      @reboot_window.launch(node_set, &callback)
    end

    # Extract some file from an archive
    #
    # Arguments
    # * archive: archive name
    # * archive_kind: kind of archive
    # * file_array: array of file to extract from the archive
    # * dest_dir: destination dir for the files extracted
    # Output
    # * return true if the file are extracted correctly, false otherwise
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


    # Copy the kernel and the initrd into the PXE directory
    #
    # Arguments
    # * files: array of file
    # Output
    # * return true if the operation is correctly performed, false
    def copy_kernel_initrd_to_pxe(files)
      must_extract = false
      archive = @config.exec_specific.environment.tarball["file"]
      dest_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
      prefix_in_cache = "e" + @config.exec_specific.environment.id + "v" + @config.exec_specific.environment.version + "--"
      files.each { |file|
        if not (File.exist?(dest_dir + "/" + prefix_in_cache + file)) then
          must_extract = true
        end
      }
      if not must_extract then
        files.each { |file|
          #If the archive has been modified, re-extraction required
          if (File.mtime(archive).to_i > File.atime(dest_dir + "/" + prefix_in_cache + file).to_i) then
            must_extract = true
          end
        }
      end
      if must_extract then
        files_in_archive = Array.new
        files.each { |file|
          files_in_archive.push("boot/" + file)
        }
        tmpdir = get_tmpdir()
        res = extract_files_from_archive(archive,
                                         @config.exec_specific.environment.tarball["kind"],
                                         files_in_archive,
                                         tmpdir)
        files.each { |file|
          src = tmpdir + "/" + file
          dst = dest_dir + "/" + prefix_in_cache + file
          res = res && File.move(src, dst)
        }
        if (Dir.rmdir(tmpdir) != 0) then
          @output.debugl(4, "Cannot remove the #{tmpdir} directory")
        end
        return res
      else
        return true
      end
    end

    # Get the name of the deployment partition
    #
    # Arguments
    # * nothing
    # Output
    # * return the name of the deployment partition
    def get_deploy_part_str
      if (@config.exec_specific.deploy_part != "") then
        return @config.cluster_specific[@cluster].block_device + @config.exec_specific.deploy_part
      else
        return @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].deploy_part
      end
    end

    # Get the number of the deployment partition
    #
    # Arguments
    # * nothing
    # Output
    # * return the number of the deployment partition
    def get_deploy_part_num
      if (@config.exec_specific.deploy_part != "") then
        return @config.exec_specific.deploy_part.to_i
      else
        return @config.cluster_specific[@cluster].deploy_part.to_i
      end
    end

    # Install Grub-legacy on the deployment partition
    #
    # Arguments
    # * kind of OS (linux, xen)
    # Output
    # * return true if the installation of Grub-legacy has been successfully performed, false otherwise
    def install_grub1_on_nodes(kind)
      root = get_deploy_part_str()
      grubpart = "hd0,#{get_deploy_part_num() - 1}"
      path = @config.common.environment_extraction_dir
      line1 = line2 = line3 = ""
      case kind
      when "linux"
        line1 = "/boot/#{@config.exec_specific.environment.kernel}"
        line2 = "/boot/#{@config.exec_specific.environment.initrd}"
      when "xen"
        line1 = "/boot/#{@config.exec_specific.environment.hypervisor} #{@config.exec_specific.environment.hypervisor_params}"
        line2 = "/boot/#{@config.exec_specific.environment.kernel}"
        line3 = "/boot/#{@config.exec_specific.environment.initrd}"
      else
        return false
      end
      return parallel_exec_command_wrapper_expecting_status("(/usr/local/bin/install_grub \
                                                            #{kind} #{root} \"#{grubpart}\" #{path} \
                                                            \"#{line1}\" \"#{line2}\" \"#{line3}\")",
                                                            ["0"],
                                                            @config.common.taktuk_connector)
    end

    # Install Grub 2 on the deployment partition
    #
    # Arguments
    # * kind of OS (linux, xen)
    # Output
    # * return true if the installation of Grub 2 has been successfully performed, false otherwise
    def install_grub2_on_nodes(kind)
      root = get_deploy_part_str()
      grubpart = "hd0,#{get_deploy_part_num()}"
      path = @config.common.environment_extraction_dir
      line1 = line2 = line3 = ""
      case kind
      when "linux"
        line1 = "/boot/#{@config.exec_specific.environment.kernel}"
        line2 = "/boot/#{@config.exec_specific.environment.initrd}"
      when "xen"
        line1 = "/boot/#{@config.exec_specific.environment.hypervisor}"
        line2 = "/boot/#{@config.exec_specific.environment.kernel}"
        line3 = "/boot/#{@config.exec_specific.environment.initrd}"
      else
        return false
      end
      return parallel_exec_command_wrapper_expecting_status("(/usr/local/bin/install_grub2 \
                                                            #{kind} #{root} \"#{grubpart}\" #{path} \
                                                            \"#{line1}\" \"#{line2}\" \"#{line3}\")",
                                                            ["0"],
                                                            @config.common.taktuk_connector)
    end


    # Send a tarball with Taktuk and uncompress it on the nodes
    #
    # Arguments
    # * scattering_kind:  kind of taktuk scatter (tree, chain)
    # * tarball_file: path to the tarball
    # * tarball_kind: kind of archive (tgz, tbz2, ddgz, ddbz2)
    # * deploy_mount_point: deploy mount point
    # * deploy_mount_part: deploy mount part
    # Output
    # * return true if the operation is correctly performed, false otherwise
    def send_tarball_and_uncompress_with_taktuk(scattering_kind, tarball_file, tarball_kind, deploy_mount_point, deploy_part)
      case tarball_kind
      when "tgz"
        cmd = "tar xz -C #{deploy_mount_point}"
      when "tbz2"
        cmd = "tar xj -C #{deploy_mount_point}"
      when "ddgz"
        cmd = "gzip -cd > #{deploy_part}"
      when "ddbz2"
        cmd = "bzip2 -cd > #{deploy_part}"
      else
        @output.debugl(0, "The #{tarball_kind} archive kind is not supported")
        return false
      end
      return parallel_exec_cmd_with_input_file_wrapper(tarball_file,
                                                       cmd,
                                                       scattering_kind,
                                                       @config.common.taktuk_connector,
                                                       "0")
    end

    # Send a tarball with Bittorrent and uncompress it on the nodes
    #
    # Arguments
    # * tarball_file: path to the tarball
    # * tarball_kind: kind of archive (tgz, tbz2, ddgz, ddbz2)
    # * deploy_mount_point: deploy mount point
    # * deploy_mount_part: deploy mount part
    # Output
    # * return true if the operation is correctly performed, false otherwise
    def send_tarball_and_uncompress_with_bittorrent(tarball_file, tarball_kind, deploy_mount_point, deploy_part)
      torrent = "#{tarball_file}.torrent"
      if not Bittorrent::make_torrent(tarball_file, @config.common.bt_tracker_ip, @config.common.bt_tracker_port) then
        @output.debugl(0, "The torrent file (#{torrent}) has not been created")
        return false
      end
      seed_pid = Bittorrent::launch_seed(torrent, @config.common.kadeploy_cache_dir)
      if (seed_pid == -1) then
        @output.debugl(0, "The seed of #{torrent} has not been launched")
        return false
      end
      if not parallel_send_file_command_wrapper(torrent, "/tmp", "tree", @config.common.taktuk_connector) then
        @output.debugl(4, "Error while sending the torrent file")
        return false
      end
      if not parallel_exec_command_wrapper("/usr/local/bin/bittorrent_detach /tmp/#{File.basename(torrent)}", @config.common.taktuk_connector) then
        @output.debugl(4, "Error while launching the bittorrent download")
        return false
      end
      sleep(10)
      if not Bittorrent::wait_end_of_download(@config.common.bt_download_timeout, torrent, @config.common.bt_tracker_ip, @config.common.bt_tracker_port) then
        @output.debugl(0, "A timeout for the bittorrent download has been reached")
        Process.kill("SIGKILL", seed_pid)
        return false
      end
      @output.debugl(4, "Shutdown the seed for #{torrent}")
      Process.kill("SIGKILL", seed_pid)
      
      case tarball_kind
      when "tgz"
        cmd = "tar xzf /tmp/#{File.basename(tarball_file)} -C #{deploy_mount_point}"
      when "tbz2"
        cmd = "tar xjf /tmp/#{File.basename(tarball_file)} -C #{deploy_mount_point}"
      when "ddgz"
        cmd = "gzip -cd /tmp/#{File.basename(tarball_file)} > #{deploy_part}"
      when "ddbz2"
        cmd = "bzip2 -cd /tmp/#{File.basename(tarball_file)} > #{deploy_part}"
      else
        @output.debugl(0, "The #{tarball_kind} archive kind is not supported")
        return false
      end
      if not parallel_exec_command_wrapper(cmd, @config.common.taktuk_connector) then
        @output.debugl(4, "Error while uncompressing the tarball")
        return false
      end
      return true
    end
   

    # Execute a custom command on the nodes
    #
    # Arguments
    # * cmd: command to execute
    # Output
    # * return true if the command has been correctly performed, false otherwise
    def custom_exec_cmd(cmd)
      @output.debugl(4, "CUS exec_cmd: #{@nodes_ok.to_s}")
      return parallel_exec_command_wrapper(cmd, @config.common.taktuk_connector)
    end

    # Send a custom file on the nodes
    #
    # Arguments
    # * file: filename
    # * dest_dir: destination directory on the nodes
    # Output
    # * return true if the file has been correctly sent, false otherwise
    def custom_send_file(file, dest_dir)
      @output.debugl(4, "CUS send_file: #{@nodes_ok.to_s}")
      return parallel_send_file_command_wrapper(file,
                                                dest_dir,
                                                "chain",
                                                @config.common.taktuk_connector)
    end

    # Run the custom methods attached to a micro step
    #
    # Arguments
    # * macro_step: name of the macro step
    # * micro_step: name of the micro step
    # Output
    # * return true if the methods have been successfully executed, false otherwise    
    def run_custom_methods(macro_step, micro_step)
      result = true
      @config.exec_specific.custom_operations[macro_step][micro_step].each { |entry|
        cmd = entry[0]
        arg = entry[1]
        dir = entry[2]
        case cmd
        when "exec"
          result = result && custom_exec_cmd(arg)
        when "send"
          result = result && custom_send_file(arg, dir)
        else
          @output.debugl(0, "Invalid custom method: #{cmd}")
          return false
        end
      }
      return result
    end

    # Check if some custom methods are attached to a micro step
    #
    # Arguments
    # * macro_step: name of the macro step
    # * micro_step: name of the micro step
    # Output
    # * return true if at least one custom method is attached to the micro step, false otherwise
    def custom_methods_attached?(macro_step, micro_step)
      return ((@config.exec_specific.custom_operations != nil) && 
              @config.exec_specific.custom_operations.has_key?(macro_step) && 
              @config.exec_specific.custom_operations[macro_step].has_key?(micro_step))
    end

    # Create a tmp directory
    #
    # Arguments
    # * nothing
    # Output
    # * return the path of the tmp directory
    def get_tmpdir
      pr = CmdCtrlWrapper::init
      CmdCtrlWrapper::add_cmd(pr, "mktemp -d", "none")
      CmdCtrlWrapper::run(pr)
      path = CmdCtrlWrapper::get_output(pr)
      return path.chomp
    end

    public

    # Test if a timeout is reached
    #
    # Arguments
    # * timeout: timeout
    # * instance_thread: instance of thread that waits for the timeout
    # * step_name: name of the current step
    # Output   
    # * return true if the timeout is reached, false otherwise
    def timeout?(timeout, instance_thread, step_name, instance_node_set)
      start = Time.now.to_i
      while ((instance_thread.status != false) && (Time.now.to_i < (start + timeout)))
        sleep(1)
      end
      if (instance_thread.status != false) then
        @output.debugl(4, "Timeout before the end of the step, let's kill the instance")
        Thread.kill(instance_thread)
        @nodes_ok.free
        instance_node_set.duplicate_and_free(@nodes_ko)
        @nodes_ko.set_error_msg("Timeout in the #{step_name} step")
        return true
      else
        instance_node_set.free()
        instance_thread.join
        return false
      end
    end

    def method_missing(method_sym, *args)
      if (@nodes_ok.empty?) then
        return false
      else
        real_method = "ms_#{method_sym.to_s}".to_sym
        if (self.class.method_defined? real_method) then
          if (@config.exec_specific.breakpoint_on_microstep == method_sym.to_s) then
            @output.debugl(0, "BRK #{method_sym.to_s}: #{@nodes_ok.to_s}")
            @config.exec_specific.breakpointed = true
            return false
          else
            if custom_methods_attached?(@macro_step, method_sym.to_s) then
              if run_custom_methods(@macro_step, method_sym.to_s) then
                @output.debugl(3, "--- #{method_sym.to_s}")
                @output.debugl(4, "  >>>  #{@nodes_ok.to_s}")
                send(real_method, *args)
              end
            else
              @output.debugl(3, "--- #{method_sym.to_s}")
              @output.debugl(4, "  >>>  #{@nodes_ok.to_s}")
              send(real_method, *args)
            end
          end
        else
          @output.debugl(0, "Wrong method: #{method_sym} #{real_method}!!!")
          exit 1
        end
      end
    end
    
    # Change the PXE configuration
    #
    # Arguments
    # * step: kind of change (prod_to_deploy_env, prod_to_nfsroot_env, chainload_pxe, back_to_prod_env)
    # * pxe_profile_msg (opt): string containing the pxe profile
    # Output
    # * return true if the operation has been performed correctly, false otherwise
    def ms_switch_pxe(step, pxe_profile_msg = "")
      case step
      when "prod_to_deploy_env"
        res = PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,   
                                               @config.cluster_specific[@cluster].deploy_kernel,
                                               @config.cluster_specific[@cluster].deploy_initrd,
                                               "",
                                               @config.common.tftp_repository,
                                               @config.common.tftp_images_path,
                                               @config.common.tftp_cfg)
      when "prod_to_nfsroot_env"
         res = PXEOperations::set_pxe_for_nfsroot(@nodes_ok.make_array_of_ip,
                                                  @config.common.nfsroot_kernel,
                                                  @config.common.nfs_server,
                                                  @config.common.tftp_repository,
                                                  @config.common.tftp_images_path,
                                                  @config.common.tftp_cfg)
      when "set_pxe"
        res = PXEOperations::set_pxe_for_custom(@nodes_ok.make_array_of_ip,
                                                pxe_profile_msg,
                                                @config.common.tftp_repository,
                                                @config.common.tftp_cfg)
      when "deploy_to_deployed_env"
        if (@config.exec_specific.pxe_profile_msg != "") then
          res = PXEOperations::set_pxe_for_custom(@nodes_ok.make_array_of_ip,
                                                  @config.exec_specific.pxe_profile_msg,
                                                  @config.common.tftp_repository,
                                                  @config.common.tftp_cfg)
        else
          case @config.common.bootloader
          when "pure_pxe"
            prefix_in_cache = "e" + @config.exec_specific.environment.id + "v" + @config.exec_specific.environment.version + "--"
            case @config.exec_specific.environment.environment_kind
            when "linux"
              kernel = prefix_in_cache + @config.exec_specific.environment.kernel
              initrd = prefix_in_cache + @config.exec_specific.environment.initrd
              images_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
              res = system("touch -a #{images_dir}/#{kernel}")
              res = res && system("touch -a #{images_dir}/#{initrd}")
              res = res && PXEOperations::set_pxe_for_linux(@nodes_ok.make_array_of_ip,
                                                            kernel,
                                                            initrd,
                                                            @config.exec_specific.environment.part,
                                                            @config.common.tftp_repository,
                                                            @config.common.tftp_images_path,
                                                            @config.common.tftp_cfg)
            when "xen"
              kernel = prefix_in_cache + @config.exec_specific.environment.kernel
              initrd = prefix_in_cache + @config.exec_specific.environment.initrd
              hypervisor = prefix_in_cache + @config.exec_specific.environment.hypervisor
              images_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
              res = system("touch -a #{images_dir}/#{kernel}")
              res = res && system("touch -a #{images_dir}/#{initrd}")
              res = res && system("touch -a #{images_dir}/#{hypervisor}")
              res = res && PXEOperations::set_pxe_for_xen(@nodes_ok.make_array_of_ip,
                                                          hypervisor,
                                                          @config.exec_specific.environment.hypervisor_params,
                                                          kernel,
                                                          @config.exec_specific.environment.kernel_params,
                                                          initrd,
                                                          @config.exec_specific.environment.part,
                                                          @config.common.tftp_repository,
                                                          @config.common.tftp_images_path,
                                                          @config.common.tftp_cfg)
            end
            Cache::clean_cache(@config.common.tftp_repository + "/" + @config.common.tftp_images_path,
                               @config.common.tftp_images_max_size * 1024 * 1024,
                               6,
                               /^.+--e\d+v\d+$/)
          when "chainload_pxe"
            if (@config.exec_specific.environment.environment_kind != "xen") then
              PXEOperations::set_pxe_for_chainload(@nodes_ok.make_array_of_ip,
                                                   get_deploy_part_num(),
                                                   @config.common.tftp_repository,
                                                   @config.common.tftp_images_path,
                                                   @config.common.tftp_cfg)
            else
              # @output.debugl(4, "Hack, Grub2 seems to failed to boot a Xen Dom0, so let's use the pure PXE fashion")
              prefix_in_cache = "e" + @config.exec_specific.environment.id + "v" + @config.exec_specific.environment.version + "--"
              kernel = prefix_in_cache + @config.exec_specific.environment.kernel
              initrd = prefix_in_cache + @config.exec_specific.environment.initrd
              hypervisor = prefix_in_cache + @config.exec_specific.environment.hypervisor
              images_dir = @config.common.tftp_repository + "/" + @config.common.tftp_images_path
              res = system("touch -a #{images_dir}/#{kernel}")
              res = res && system("touch -a #{images_dir}/#{initrd}")
              res = res && system("touch -a #{images_dir}/#{hypervisor}")
              res = res && PXEOperations::set_pxe_for_xen(@nodes_ok.make_array_of_ip,
                                                          hypervisor,
                                                          @config.exec_specific.environment.hypervisor_params,
                                                          kernel,
                                                          @config.exec_specific.environment.kernel_params,
                                                          initrd,
                                                          @config.exec_specific.environment.part,
                                                          @config.common.tftp_repository,
                                                          @config.common.tftp_images_path,
                                                          @config.common.tftp_cfg)
              Cache::clean_cache(@config.common.tftp_repository + "/" + @config.common.tftp_images_path,
                                 @config.common.tftp_images_max_size * 1024 * 1024,
                                 6,
                                 /^.+--e\d+v\d+$/)              
            end
          end
        end
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
        @output.debugl(0, "The PXE configuration has not been performed correctly: #{step}")
      end
      return res
    end

    # Perform a reboot on the current set of nodes_ok
    #
    # Arguments
    # * reboot_kind: kind of reboot (soft, hard, very_hard, kexec)
    # * use_rsh_for_reboot (opt): specify if rsh must be used for soft reboot
    # Output
    # * return true (should be false sometimes :D)
    def ms_reboot(reboot_kind, use_rsh_for_reboot = false)
      case reboot_kind
      when "soft"
        reboot_wrapper("soft", use_rsh_for_reboot)
      when "hard"
        reboot_wrapper("hard")
      when "very_hard"
        reboot_wrapper("very_hard")
      when "kexec"
        if (@config.exec_specific.environment.environment_kind == "linux") then
          kernel = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.kernel}"
          initrd = "#{@config.common.environment_extraction_dir}/boot/#{@config.exec_specific.environment.initrd}"
          kernel_params = @config.exec_specific.environment.kernel_params
          root_part = @config.exec_specific.environment.part
          #Warning, this require the /usr/local/bin/kexec_detach script
          return parallel_exec_command_wrapper("(/usr/local/bin/kexec_detach #{kernel} #{initrd} #{root_part} #{kernel_params})",
                                               @config.common.taktuk_connector)
        else
          @output.debugl(4, "The Kexec optimization can only be used with a linux environment")
          reboot_wrapper("soft", use_rsh_for_reboot)
        end
      end
      return true
    end

    # Check the state of a set of nodes
    #
    # Arguments
    # * step: step in which the nodes are expected to be
    # Output
    # * return true if the check has been successfully performed, false otherwise
    def ms_check_nodes(step)
      case step
      when "deployed_env_booted"
        #we look if the / mounted partition is the deployment partition
        return parallel_exec_command_wrapper_expecting_status_and_output("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )",
                                                                         ["0"],
                                                                         @config.exec_specific.environment.part,
                                                                         @config.common.taktuk_connector)
      when "prod_env_booted"
        #we look if the / mounted partition is the default production partition
        return parallel_exec_command_wrapper_expecting_status_and_output("(mount | grep \\ \\/\\  | cut -f 1 -d\\ )",
                                                                         ["0"],
                                                                         @config.cluster_specific[@cluster].block_device + \
                                                                         @config.cluster_specific[@cluster].prod_part,
                                                                         @config.common.taktuk_connector)
      end
    end

    # Load some specific drivers on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the drivers have been successfully loaded, false otherwise
    def ms_load_drivers
      cmd = String.new
      @config.cluster_specific[@cluster].drivers.each_index { |i|
        cmd += "modprobe #{@config.cluster_specific[@cluster].drivers[i]};"
      }
      return parallel_exec_command_wrapper(cmd, @config.common.taktuk_connector)
    end

    # Perform a fdisk on the ndoes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the fdisk has been successfully performed, false otherwise
    def ms_fdisk(kind)
      case kind
      when "prod_env"
        expected_status = "256" #Strange thing, fdisk can not reload the partition table so it exits with 256
      when "untrusted_env"
        expected_status = "0"
      else
        @output.debugl(0, "Invalid kind of deploy environment: #{kind}")
        return false
      end
      temp = Tempfile.new("fdisk_#{@cluster}")
      system("cat #{@config.cluster_specific[@cluster].fdisk_file}|sed 's/PARTTYPE/#{@config.exec_specific.environment.fdisk_type}/' > #{temp.path}")
      res = parallel_exec_cmd_with_input_file_wrapper(temp.path,
                                                      "fdisk #{@config.cluster_specific[@cluster].block_device}",
                                                      "tree",
                                                      @config.common.taktuk_connector,
                                                      expected_status)
      temp.unlink
      return res
    end

    # Perform the deployment part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the format has been successfully performed, false otherwise
    def ms_format_deploy_part
      if ((@config.exec_specific.environment.tarball["kind"] == "tgz") ||
          (@config.exec_specific.environment.tarball["kind"] == "tbz2")) then
        if @config.common.mkfs_options.has_key?(@config.exec_specific.environment.filesystem) then
          opts = @config.common.mkfs_options[@config.exec_specific.environment.filesystem]
          return parallel_exec_command_wrapper("mkdir -p #{@config.common.environment_extraction_dir}; \
                                              umount #{get_deploy_part_str()} 2>/dev/null; \
                                              mkfs -t #{@config.exec_specific.environment.filesystem} #{opts} #{get_deploy_part_str()}",
                                               @config.common.taktuk_connector)
        else
          return parallel_exec_command_wrapper("mkdir -p #{@config.common.environment_extraction_dir}; \
                                              umount #{get_deploy_part_str()} 2>/dev/null; \
                                              mkfs -t #{@config.exec_specific.environment.filesystem} #{get_deploy_part_str()}",
                                               @config.common.taktuk_connector)
        end
      else
        @output.debugl(4, "Bypass the format of the deploy part")
        return true
      end
    end

    # Format the /tmp part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the format has been successfully performed, false otherwise
    def ms_format_tmp_part
      if (@config.exec_specific.reformat_tmp) then
        if @config.common.mkfs_options.has_key?("ext2") then
          opts = @config.common.mkfs_options["ext2"]
          tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
          return parallel_exec_command_wrapper("mkdir -p /tmp; umount #{tmp_part} 2>/dev/null; mkfs.ext2 #{opts} #{tmp_part}",
                                               @config.common.taktuk_connector)
        else
          tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
          return parallel_exec_command_wrapper("mkdir -p /tmp; umount #{tmp_part} 2>/dev/null; mkfs.ext2 #{tmp_part}",
                                               @config.common.taktuk_connector)
        end
      end
      return true
    end

    # Mount the deployment part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the mount has been successfully performed, false otherwise
    def ms_mount_deploy_part
      #we do not mount the deploy part for a dd.gz or dd.bz2 image
      if ((@config.exec_specific.environment.tarball["kind"] == "tgz") ||
          (@config.exec_specific.environment.tarball["kind"] == "tbz2")) then
        return parallel_exec_command_wrapper("mount #{get_deploy_part_str()} #{@config.common.environment_extraction_dir}",
                                             @config.common.taktuk_connector)
      else
        @output.debugl(4, "Bypass the mount of the deploy part")
        return true
      end
    end

    # Mount the /tmp part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the mount has been successfully performed, false otherwise
    def ms_mount_tmp_part
      tmp_part = @config.cluster_specific[@cluster].block_device + @config.cluster_specific[@cluster].tmp_part
      return parallel_exec_command_wrapper("mount #{tmp_part} /tmp",
                                           @config.common.taktuk_connector)
    end


    # Send SSH keys ont the nodes
    #
    # Arguments
    # * scattering_kind:  kind of taktuk scatter (tree, chain)
    # Output
    # * return true if the keys have been successfully copied, false otherwise
    def ms_send_key(scattering_kind)
      if (@config.exec_specific.key != "") then
        cmd = "cat - >>#{@config.common.environment_extraction_dir}/root/.ssh/authorized_keys"
        return parallel_exec_cmd_with_input_file_wrapper(@config.exec_specific.key,
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
    # * ports_up: up ports used to perform a reach test on the nodes
    # * ports_down: down ports used to perform a reach test on the nodes
    # Output
    # * return true if some nodes are here, false otherwise
    def ms_wait_reboot(ports_up, ports_down)
      return parallel_wait_nodes_after_reboot_wrapper(@config.cluster_specific[@cluster].timeout_reboot, 
                                                      ports_up, 
                                                      ports_down,
                                                      @nodes_check_window)
    end
    
    # Eventually install a bootloader
    #
    # Arguments
    # * nothing
    # Output
    # * return true if case of success (the success should be tested better)
    def ms_install_bootloader
      case @config.common.bootloader
      when "pure_pxe"
        case @config.exec_specific.environment.environment_kind
        when "linux"
          return copy_kernel_initrd_to_pxe([@config.exec_specific.environment.kernel,
                                            @config.exec_specific.environment.initrd])
        when "xen"
          return copy_kernel_initrd_to_pxe([@config.exec_specific.environment.kernel,
                                            @config.exec_specific.environment.initrd,
                                            @config.exec_specific.environment.hypervisor])
        when "other"
          @output.debugl(0, "Only linux and xen environments can be booted with a pure PXE configuration")
          return false
        end
      when "chainload_pxe"
        if @config.exec_specific.disable_bootloader_install then
          @output.debugl(4, "Bypass the bootloader installation")
          return true
        else
          case @config.exec_specific.environment.environment_kind
          when "linux"
            return install_grub2_on_nodes("linux")
          when "xen"
#           return install_grub2_on_nodes("xen")
            @output.debugl(4, "Hack, Grub2 seems to failed to boot a Xen Dom0, so let's use the pure PXE fashion")
            return copy_kernel_initrd_to_pxe([@config.exec_specific.environment.kernel,
                                              @config.exec_specific.environment.initrd,
                                              @config.exec_specific.environment.hypervisor])

          when "other"
            #in this case, the bootloader must be installed by the user (dd partition)
            return true
          end
        end
      else
        @output.debugl(0, "Invalid bootloader value: #{@config.common.bootloader}")
        return false
      end
    end

    # Dummy method to put all the nodes in the node_ko set
    #
    # Arguments
    # * nothing
    # Output
    # * return true (should be false sometimes :D)
    def ms_produce_bad_nodes
      @nodes_ok.duplicate_and_free(@nodes_ko)
      return true
    end

    # Umount the deployment part on the nodes
    #
    # Arguments
    # * nothing
    # Output
    # * return true if the deploy part has been successfully umounted, false otherwise
    def ms_umount_deploy_part
      if ((@config.exec_specific.environment.tarball["kind"] == "tgz") ||
          (@config.exec_specific.environment.tarball["kind"] == "tbz2")) then
        return parallel_exec_command_wrapper("umount #{@config.exec_specific.environment.part}", @config.common.taktuk_connector)
      else
        @output.debugl(4, "Bypass the umount of the deploy part")
        return true
      end
    end

    # Send and uncompress the user environment on the nodes
    #
    # Arguments
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * return true if the environment has been successfully uncompressed, false otherwise
    def ms_send_environment(scattering_kind)
      if  (scattering_kind == "bittorrent") then
        return send_tarball_and_uncompress_with_bittorrent(@config.exec_specific.environment.tarball["file"],
                                                           @config.exec_specific.environment.tarball["kind"],
                                                           @config.common.environment_extraction_dir,
                                                           @config.exec_specific.environment.part)
      else
        return send_tarball_and_uncompress_with_taktuk(scattering_kind,
                                                       @config.exec_specific.environment.tarball["file"],
                                                       @config.exec_specific.environment.tarball["kind"],
                                                       @config.common.environment_extraction_dir,
                                                       @config.exec_specific.environment.part)
      end
    end

    # Send and execute the admin preinstalls on the nodes
    #
    # Arguments
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * return true if the admin preinstall has been successfully uncompressed, false otherwise
    def ms_manage_admin_pre_install(scattering_kind)
      res = true
      @config.cluster_specific[@cluster].admin_pre_install.each { |preinstall|
        res = res && send_tarball_and_uncompress_with_taktuk(scattering_kind, preinstall["file"], preinstall["kind"], @config.common.rambin_path, "")
        if (preinstall["script"] == "breakpoint") then
          @output.debugl(0, "Breakpoint on admin preinstall after sending the file #{preinstall["file"]}")
          @config.exec_specific.breakpointed = true
          res= false
        else
          res = res && parallel_exec_command_wrapper("(KADEPLOY_CLUSTER=\"#{@cluster}\" KADEPLOY_ENV=\"#{@config.exec_specific.environment.name}\" #{@config.common.rambin_path}/#{preinstall["script"]})",
                                                     @config.common.taktuk_connector)
        end
      }
      return res
    end

    # Send and execute the admin postinstalls on the nodes
    #
    # Arguments
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * return true if the admin postinstall has been successfully uncompressed, false otherwise   
    def ms_manage_admin_post_install(scattering_kind)
      res = true
      if (@config.exec_specific.environment.environment_kind != "other") then
        @config.cluster_specific[@cluster].admin_post_install.each { |postinstall|
          res = res && send_tarball_and_uncompress_with_taktuk(scattering_kind, postinstall["file"], postinstall["kind"], @config.common.rambin_path, "")
          if (postinstall["script"] == "breakpoint") then 
            @output.debugl(0, "Breakpoint on admin postinstall after sending the file #{postinstall["file"]}")         
            @config.exec_specific.breakpointed = true
            res= false
# Note: with the current prepost install used in Grid'5000, no script must be executed at the admin post-install
# since it is only a set of variable
#          else
#            res = res && parallel_exec_command_wrapper("(KADEPLOY_CLUSTER=\"#{@cluster}\" KADEPLOY_ENV=\"#{@config.exec_specific.environment.name}\" #{@config.common.rambin_path}/#{postinstall["script"]})",
#                                                       @config.common.taktuk_connector)
          end
        }
      else
        @output.debugl(4, "Bypass the admin postinstalls")
      end
      return res
    end

    # Send and execute the user postinstalls on the nodes
    #
    # Arguments
    # * scattering_kind: kind of taktuk scatter (tree, chain)
    # Output
    # * return true if the user postinstall has been successfully uncompressed, false otherwise
    def ms_manage_user_post_install(scattering_kind)
      res = true
      if (@config.exec_specific.environment.environment_kind != "other") then
        @config.exec_specific.environment.postinstall.each { |postinstall|
          res = res && send_tarball_and_uncompress_with_taktuk(scattering_kind, postinstall["file"], postinstall["kind"], @config.common.rambin_path, "")
          if (postinstall["script"] == "breakpoint") then
            @output.debugl(0, "Breakpoint on user postinstall after sending the file #{postinstall["file"]}")
            @config.exec_specific.breakpointed = true
            res= false
          else
            res = res && parallel_exec_command_wrapper("(KADEPLOY_CLUSTER=\"#{@cluster}\" KADEPLOY_ENV=\"#{@config.exec_specific.environment.name}\" #{@config.common.rambin_path}/#{postinstall["script"]})",
                                                       @config.common.taktuk_connector)
          end
        }
      else
        @output.debugl(4, "Bypass the user postinstalls")
      end
      return res
    end
  end
end
