require 'contrib/taktuk_wrapper'
require 'yaml'

module ParallelOperations
  class ParallelOps
    @nodes = nil

    def initialize(nodes)
      @nodes = nodes
    end

    def make_taktuk_cmd(cmd)
      args = String.new
      args = "-s"
      @nodes.set.each { |node|
        args += " -m #{node.hostname}"
      }
      args += " broadcast exec [ #{cmd} ]"
      return args.split(" ")
    end

    def get_command_infos(tw)
      tree = YAML.load((YAML.dump({"hosts"=>tw.hosts,
                                    "connectors"=>tw.connectors,
                                    "errors"=>tw.errors,
                                    "infos"=>tw.infos})))
      rank = 0
      tree['hosts'].each_value { |h|
        h['commands'].each_value { |x|
          @nodes.set[rank].last_cmd_exit_status = x['status']
          @nodes.set[rank].last_cmd_stdout = x['output']
          @nodes.set[rank].last_cmd_stderr = x['error']
        }
        rank += 1
      }
    end

    def execute(cmd)
      tw = TaktukWrapper::new(make_taktuk_cmd(cmd))
      tw.run
      get_command_infos(tw)
      good_nodes = Array.new
      bad_nodes = Array.new

      @nodes.set.each { |node|
        if node.last_cmd_exit_status == '0' then
          good_nodes.push(node)
        else
          bad_nodes.push(node)
        end
      }
      return [good_nodes, bad_nodes]
    end
  end
end
