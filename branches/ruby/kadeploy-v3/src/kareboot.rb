#!/usr/bin/ruby -w

#Kadeploy libs
require 'config'
require 'db'
require 'checkrights'

#Ruby libs
require 'drb'

class KarebootClient
  @kadeploy_server = nil

  def initialize(kadeploy_server)
    @kadeploy_server = kadeploy_server
  end
  
  # Print a message (RPC)
  #
  # Arguments
  # * msg: string to print
  # Output
  # * prints a message
  def print(msg)
    puts msg
  end

  # Stops the DRB service and to release the client (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * nothing
  def exit
    DRb.stop_service()
  end
end

#Connect to the Kadeploy server to get the common configuration
client_config = ConfigInformation::Config.load_client_config_file
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
common_config = kadeploy_server.get_common_config

config = ConfigInformation::Config.new("kareboot", common_config.nodes_desc)
config.common = common_config


if (config.check_config("kareboot") == true)
  db = Database::DbFactory.create(config.common.db_kind)
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)

  #Rights check
  allowed_to_deploy = true
  part = String.new
  #The rights must be checked for each cluster if the node_list contains nodes from several clusters
  config.exec_specific.node_list.group_by_cluster.each_pair { |cluster, set|
    part = kadeploy_server.get_default_deploy_part(cluster)
    allowed_to_deploy = CheckRights::CheckRightsFactory.create(config.common.rights_kind,
                                                               set,
                                                               db,
                                                               part).granted?
  }
  
  if allowed_to_deploy then
    #Launch the listener on the client
    kareboot_client = KarebootClient.new(kadeploy_server)
    DRb.start_service(nil, kareboot_client)
    if /druby:\/\/([\w+.+]+):(\w+)/ =~ DRb.uri
      content = Regexp.last_match
      client_host = content[1]
      client_port= content[2]
    else
      puts "The URI #{DRb.uri} is not correct"
      exit(1)
    end

    if (config.exec_specific.debug_level != "") then
      debug_level = config.exec_specific.debug_level
    else
      debug_level = nil
    end
    pxe_profile_msg = String.new
    if (config.exec_specific.pxe_profile_file != "") then
      IO.readlines(config.exec_specific.pxe_profile_file).each { |l|
        pxe_profile_msg.concat(l)
      }
    end
    kadeploy_server.launch_reboot(config.exec_specific.node_list, config.exec_specific.reboot_kind,
                                  client_host, client_port, debug_level, pxe_profile_msg)
  else
    puts "You do not have the deployment rights on all the nodes"
  end

  db.disconnect
end
