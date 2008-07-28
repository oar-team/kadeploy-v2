#!/usr/bin/ruby -w

#Kadeploy libs
require 'lib/checkrights'
require 'lib/config'

#Ruby libs
require 'thread'
require 'drb'
require 'socket'


class KadeployClient
  @kadeploy_server = nil

  def initialize(kadeploy_server)
    @kadeploy_server = kadeploy_server
  end

  def print(msg)
    puts msg
  end

  def exit
    DRb.stop_service()
  end
  
  def get_file(file_name)
    @kadeploy_server.pre_send_file(File.basename(file_name))
    sock = TCPSocket.new(@kadeploy_server.dest_host, @kadeploy_server.dest_port)
    file = File.open(file_name)
    while (buf = file.read(@kadeploy_server.tcp_buffer_size))
      sock.send(buf, 0)
    end
    @kadeploy_server.post_send_file
  end
end

client_config = ConfigInformation::Config.load_client_config_file

#Connect to the server
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
puts "Successfully connected on the Kadeploy server on #{client_config.kadeploy_server}"
common_config = kadeploy_server.get_common_config
exec_specific_config = ConfigInformation::Config.load_kadeploy_exec_specific(common_config.nodes_desc)

#Launch the listener on the client
kadeploy_client = KadeployClient.new(kadeploy_server)
DRb.start_service(nil, kadeploy_client)
if /druby:\/\/([\w+.+]+):(\w+)/ =~ DRb.uri
  content = Regexp.last_match
  client_host = content[1]
  client_port= content[2]
else
  puts "The URI #{DRb.uri} is not correct"
  exit(1)
end


if (exec_specific_config != nil) then
  kadeploy_server.set_exec_specific_config(exec_specific_config)
  kadeploy_server.launch_workflow(client_host, client_port)
end
