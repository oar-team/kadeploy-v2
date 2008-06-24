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

    def parallel_command_wrapper(cmd)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      po = ParallelOperations::ParallelOps.new(node_set)
      classify_nodes(po.execute(cmd))
    end


    def switch_pxe(kind)
      ops = PXEOperations::PXEOps.new
      case kind
      when /prod_to_deploy_env/
        ops.set_pxe_for_linux(@nodes_ok.make_array_of_ip,
                              @config.cluster_specific[@cluster].deploy_kernel,
                              @config.cluster_specific[@cluster].deploy_initrd,
                              @config.cluster_specific[@cluster].deploy_parts[0],
                              @config.common.tftp_repository,
                              @config.common.tftp_images_path,
                              @config.common.tftp_cfg)
      when /deploy_to_deployed_env/
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
      when /soft/
        parallel_command_wrapper("date -rrr") #invalid command for test
      when /hard/
      when /veryhard/
      end
    end

    def check_nodes(kind)
      node_set = Nodes::NodeSet.new
      @nodes_ok.duplicate_and_free(node_set)
      case kind
      when /deploy_env_booted/
      when /deployed_env_booted/
      when /prod_env_booted/
      end
      node_set.duplicate_and_free(@nodes_ok)
    end

    def load_drivers
    end

    def do_fdisk
    end

    def do_format

    end
  end
end
