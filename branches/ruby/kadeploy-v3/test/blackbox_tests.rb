#!/usr/bin/ruby -w
require 'lib/cmdctrl_wrapper'
require 'lib/nodes'
require 'optparse'

KADEPLOY="/home/ejeanvoi/work/kadeploy-svn/branches/ruby/kadeploy-v3/kadeploy_client.rb"
ENVIRONMENT="etch test"
TEST_CMD="/test_env.sh"
EXPECTED_VALUE="Cool!"

def load_cmdline_options
  nodes = Array.new
  key = String.new
  progname = File::basename($PROGRAM_NAME)
  opts = OptionParser::new do |opts|
    opts.summary_indent = "  "
    opts.summary_width = 28
    opts.program_name = progname
    opts.banner = "Usage: #{progname} [options]"
    opts.separator "Contact: Emmanuel Jeanvoine <emmanuel.jeanvoine@inria.fr>"
    opts.separator ""
    opts.separator "General options:"
    opts.on("-m", "--machine MACHINE", "Node to run on") { |hostname|
      nodes.push(hostname)
    }
    opts.on("-f", "--file MACHINELIST", "Files containing list of nodes")  { |f|
      IO.readlines(f).sort.uniq.each { |hostname|
        nodes.push(hostname.chomp)
      }
    }
    opts.on("-k", "--key FILE", "Public key to copy in the root's authorized_keys") { |f|
      if not File.exist?(f) then
        puts "The file #{f} does not exist"
        return []
      end
      key = File.expand_path(f)
    }
  end
  opts.parse!(ARGV)
  return nodes, key
end


def test_deploy(nodes, step1, step2, step3, tag)
  puts "### Launching #{tag} test ###"
  File.delete("nodes_ok") if File.exist?("nodes_ok")
  node_list = String.new
  nodes.each { |node|
    node_list += " -m #{node}"
  }
  cmd = "#{KADEPLOY} #{node_list} -e \"#{ENVIRONMENT}\" -d 0 -z \"SetDeploymentEnv|#{step1}&BroadcastEnv|#{step2}&BootNewEnv|#{step3}\""
  system(cmd)
  if File.exist?("nodes_ko") then
    IO.readlines("nodes_ko").each { |node|
      puts "The node #{node} has not been correctly deployed"
    }
  else
    if File.exist?("nodes_ok") then
      puts "[ PASSED ]"
    else
      puts "[ ERROR ]"
    end
  end
end    

def test_connection(nodes, step1, step2, step3, key)
  puts "### Launching Connection test ###"
  File.delete("nodes_ok") if File.exist?("nodes_ok")
  node_list = String.new
  nodes.each { |node|
    node_list += " -m #{node}"
  }
  cmd = "#{KADEPLOY} #{node_list} -e \"#{ENVIRONMENT}\" -d 0 -k #{key} -z \"SetDeploymentEnv|#{step1}&BroadcastEnv|#{step2}&BootNewEnv|#{step3}\""
  system(cmd)
  if File.exist?("nodes_ko") then
    IO.readlines("nodes_ko").each { |node|
      puts "The node #{node} has not been correctly deployed"
    }
  end
  if File.exist?("nodes_ok") then
    deployed_nodes = Array.new
    IO.readlines("nodes_ok").each { |node|
      deployed_nodes.push(node.chomp)
    }
    pr = CmdCtrlWrapper::init
    deployed_nodes.each { |node|
      cmd = "ssh -q -o BatchMode=yes -o StrictHostKeyChecking=no -o PreferredAuthentications=publickey -o ConnectTimeout=2 root@#{node} \"#{TEST_CMD}\""
      node_instance = Nodes::Node.new(node,"","","")
      CmdCtrlWrapper::add_cmd(pr, cmd, node_instance)
    }
    CmdCtrlWrapper::run(pr)
    res = CmdCtrlWrapper::get_results_expecting_output(pr, EXPECTED_VALUE)
    if (res[1].length > 0) then
      res[1].each { |node_instance|
        puts "Error on the node #{node_instance.hostname}"
        puts "[ ERROR ]"
      }
    else
      puts "[ PASSED ]"
    end
  end
end


nodes, key = load_cmdline_options
if nodes.empty? then
  puts "You must specify at least on node, use --help option for correct use"
else
  test_deploy(nodes, "SetDeploymentEnvDummy:1:10", "BroadcastEnvDummy:1:10", "BootNewEnvDummy:1:10", "Dummy")
  test_deploy(nodes, "SetDeploymentEnvProd:2:100", "BroadcastEnvChainWithFS:2:300", "BootNewEnvKexec:1:150", "ProdEnv - Kexec reboot")
  test_deploy(nodes, "SetDeploymentEnvUntrusted:2:200", "BroadcastEnvChainWithFS:2:300", "BootNewEnvKexec:1:150", "UntrustedEnv - Kexec reboot")
  test_deploy(nodes, "SetDeploymentEnvUntrusted:2:200", "BroadcastEnvChainWithFS:2:300", "BootNewEnvClassical:1:150", "UntrustedEnv - Classical reboot")
  test_connection(nodes, "SetDeploymentEnvUntrusted:2:200", "BroadcastEnvChainWithFS:2:300", "BootNewEnvKexec:1:150", key) if (key != "")
end
