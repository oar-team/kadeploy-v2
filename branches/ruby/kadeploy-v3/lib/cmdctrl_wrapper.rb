require "cmdctrl/prunner"
include CmdCtrl
include CmdCtrl::Commands

module CmdCtrlWrapper
  class Cmd
    @pr = nil
    
    def initialize
      @pr = CmdCtrl::ParallelRunner.new
    end

    def add_cmd(cmd, node)
      @pr.commands << CommandBufferer.new(Command.new(cmd), node)
    end

    def run
      @pr.run
      @pr.wait
    end

    def get_results
      good_nodes = Array.new
      bad_nodes = Array.new
      @pr.results.each_pair { |cb, result|
        node = cb.data
        node.last_cmd_exit_status = result.status.exitstatus
        node.last_cmd_stdout = result.stdout
        node.last_cmd_stderr = result.stderr
        if result.status.exitstatus == 0 then
          good_nodes.push(cb.data)
        else
          bad_nodes.push(cb.data)
        end
      }
      return [good_nodes, bad_nodes]
    end
  end
end
