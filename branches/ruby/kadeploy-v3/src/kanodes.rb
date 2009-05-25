#!/usr/bin/ruby -w

#Kadeploy libs
require 'config'
require 'db'

#Ruby libs
require 'drb'



# List the deploy information about the nodes
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * prints the information about the nodes in a CSV format
def get_deploy_state(config, db)
  if config.exec_specific.node_list.empty? then
    query = "SELECT nodes.hostname, nodes.state, environments.name, environments.version, environments.user \
             FROM nodes, environments \
             WHERE nodes.env_id = environments.id \
             ORDER BY nodes.hostname"
  else
    nodes = String.new
    config.exec_specific.node_list.each_index { |i|
      nodes += "nodes.hostname=\"#{config.exec_specific.node_list[i]}\" "
      nodes += "OR " if (i < config.exec_specific.node_list.length - 1)
    }

    query = "SELECT nodes.hostname, nodes.state, environments.name, environments.version, environments.user \
             FROM nodes, environments \
             WHERE nodes.env_id = environments.id \
             AND (#{nodes})
             ORDER BY nodes.hostname"
  end
  res = db.run_query(query)
  res.each { |row|
    puts "#{row[0]},#{row[1]},#{row[2]},#{row[3]},#{row[4]}"
  }
end

# Get a YAML output of the current deployments
#
# Arguments
# * kadeploy_server: pointer to the Kadeploy server (DRbObject)
# * wid: workflow id
# Output
# * prints the YAML output of the current deployments
def get_yaml_dump(kadeploy_server, wid)
  puts kadeploy_server.get_workflow_state(wid)
end

def _exit(exit_code, dbh)
  dbh.disconnect if (dbh != nil)
  exit(exit_code)
end

begin
  config = ConfigInformation::Config.new("kanodes")
rescue
  _exit(1, nil)
end

#Connect to the Kadeploy server to get the common configuration
client_config = ConfigInformation::Config.load_client_config_file
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
config.common = kadeploy_server.get_common_config

if (config.check_config("kanodes") == true) then
  db = Database::DbFactory.create(config.common.db_kind)
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)
  case config.exec_specific.operation
  when "get_deploy_state"
    get_deploy_state(config, db)
  when "get_yaml_dump"
    get_yaml_dump(kadeploy_server, config.exec_specific.wid)
  end
  _exit(0, db)
else
  _exit(1, db)
end
