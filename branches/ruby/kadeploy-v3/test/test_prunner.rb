#!/usr/bin/ruby -w

require "cmdctrl/prunner"
#require "commands/command_bufferer"
include CmdCtrl
include CmdCtrl::Commands

pr = CmdCtrl::ParallelRunner.new
pr.commands << CommandBufferer::new(Command::new("date -rrr"))
pr.commands << CommandBufferer::new(Command::new("sleep 2"))
pr.commands << CommandBufferer::new(Command::new("sleep 3"))
pr.commands << CommandBufferer::new(Command::new("sleep 4"))
pr.commands << CommandBufferer::new(Command::new("date"))

pr.run
pr.wait


pr.results.each_pair { |k, r|
  puts k.cmd
  puts r.status.exitstatus.class
 # rg = ParallelRunnerResultGroup::new(r.stdout, r.stderr, r.status, r.command, 0)
  #puts rg
}
