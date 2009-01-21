#!/usr/bin/ruby -w

#Kadeploy libs
require 'config'
require 'db'
require 'nodes'
require 'checkrights'

#Ruby libs
require 'drb'

CHECK_RIGHTS_INTERVAL=60


def _exit(exit_code, dbh)
  dbh.disconnect if (dbh != nil)
  exit(exit_code)
end



#Connect to the Kadeploy server to get the common configuration
client_config = ConfigInformation::Config.load_client_config_file
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)

common_config = kadeploy_server.get_common_config
config = ConfigInformation::Config.new("kaconsole", common_config.nodes_desc)
config.common = common_config

if (config.check_config("kaconsole") == true) then
  db = Database::DbFactory.create(config.common.db_kind)
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)
  
  part = kadeploy_server.get_default_deploy_part(config.exec_specific.node.cluster)
  set = Nodes::NodeSet.new
  set.push(config.exec_specific.node)
  if (CheckRights::CheckRightsFactory.create(common_config.rights_kind, set, db, part).granted?) then
    pid = Process.fork {
      exec(config.exec_specific.node.cmd.console)
    }
    state = "running"
    while ((CheckRights::CheckRightsFactory.create(common_config.rights_kind, set, db, part).granted?) &&
           (state == "running"))
      CHECK_RIGHTS_INTERVAL.times {
        if (Process.waitpid(pid, Process::WNOHANG) == pid) then
          state = "reaped"
          break
        else
          sleep 1
        end
      }
    end
    if (state == "running") then
      Process.kill("SIGKILL", pid)
      puts "Console killed"
    end
  else
    _exit(1, db)
  end
  _exit(0, db)
else
  _exit(1, db)
end
