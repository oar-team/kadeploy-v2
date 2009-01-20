#Contrib libs
require 'cmdctrl/prunner'
include CmdCtrl
include CmdCtrl::Commands

module CmdCtrlWrapper
  # Initialize a ParallelRunner for CmdCtrl
  #
  # Arguments
  # * nothing
  # Output
  # * returns an instance of ParallelRunner
  def CmdCtrlWrapper::init
    return CmdCtrl::ParallelRunner.new
  end

  # Add a command to the ParallelRunner
  #
  # Arguments
  # * pr: instance of a ParallelRunner
  # * cmd: string of the command to add
  # * node: hostname of the node in which the command will be applied
  # Output
  # * nothing
  def CmdCtrlWrapper::add_cmd(pr, cmd, node)
    pr.commands << CommandBufferer.new(Command.new(cmd), node)
  end

  # Run a set of commands
  #
  # Arguments
  # * pr: instance of a ParallelRunner
  # Output
  # * nothing
  def CmdCtrlWrapper::run(pr)
    pr.run
    pr.wait
  end

  # Get the results of the execution
  #
  # Arguments
  # * pr: instance of a ParallelRunner
  # Output
  # * Array of two arrays ([0] contains the nodes OK and [1] contains the nodes KO)
  def CmdCtrlWrapper::get_results(pr)
    good_nodes = Array.new
    bad_nodes = Array.new
    pr.results.each_pair { |cb, result|
      node = cb.data
      node.last_cmd_exit_status = result.status.exitstatus
      node.last_cmd_stdout = result.stdout
      node.last_cmd_stderr = result.stderr
      if result.status.exitstatus == 0 then
        good_nodes.push(node)
      else
        bad_nodes.push(node)
      end
    }
    return [good_nodes, bad_nodes]
  end

  # Get the results of the execution
  #
  # Arguments
  # * pr: instance of a ParallelRunner
  # * output: string that contains the expected output
  # Output
  # * Array of two arrays ([0] contains the nodes OK and [1] contains the nodes KO)
  def CmdCtrlWrapper::get_results_expecting_output(pr, output)
    good_nodes = Array.new
    bad_nodes = Array.new
    pr.results.each_pair { |cb, result|
      node = cb.data
      node.last_cmd_exit_status = result.status.exitstatus
      node.last_cmd_stdout = result.stdout
      node.last_cmd_stderr = result.stderr
      if ((result.status.exitstatus == 0) && (result.stdout.split("\n")[0] == output)) then
        good_nodes.push(node)
      else
        bad_nodes.push(node)
      end
    }
    return [good_nodes, bad_nodes]
  end
end