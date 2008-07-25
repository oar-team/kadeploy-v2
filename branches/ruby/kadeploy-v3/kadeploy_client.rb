#!/usr/bin/ruby -w

#Kadeploy libs
require 'lib/checkrights'
require 'lib/config'

#Ruby libs
require 'thread'
require 'drb'



class KadeployClient
  def print(msg)
    puts msg
  end

  def exit
    DRb.stop_service()
  end
end


DRb.start_service(nil, KadeployClient.new)
if /druby:\/\/([\w+.+]+):(\w+)/ =~ DRb.uri
  content = Regexp.last_match
  client_host = content[1]
  client_port= content[2]
else
  puts "The URI #{DRb.uri} is not correct"
  exit(1)
end

client_config = ConfigInformation::Config.load_client_config_file

DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
puts "Successfully connected on the Kadeploy server on #{client_config.kadeploy_server}"
common_config = kadeploy_server.get_common_config
exec_specific_config = ConfigInformation::Config.load_kadeploy_exec_specific(common_config.nodes_desc)
if (exec_specific_config != nil) then
  kadeploy_server.set_exec_specific_config(exec_specific_config)
  kadeploy_server.launch_workflow(client_host, client_port)
end
