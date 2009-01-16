#!/usr/bin/ruby -w

#Kadeploy libs
require 'checkrights'
require 'config'
require 'db'

#Ruby libs
require 'thread'
require 'drb'
require 'socket'


class KadeployClient
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

  # Stop the DRB service and to release the client (RPC)
  #
  # Arguments
  # * nothing
  # Output
  # * nothing
  def exit
    DRb.stop_service()
  end

  # Get a file from the client (RPC)
  #
  # Arguments
  # * file_name: name of the file on the client side
  # Output
  # * nothing  
  def get_file(file_name)
    @kadeploy_server.pre_send_file(File.basename(file_name))
    sock = TCPSocket.new(@kadeploy_server.dest_host, @kadeploy_server.dest_port)
    file = File.open(file_name)
    while (buf = file.read(@kadeploy_server.tcp_buffer_size))
      sock.send(buf, 0)
    end
    sock.close
    @kadeploy_server.post_send_file
  end

  # Print the results of the deployment (RPC)
  #
  # Arguments
  # * nodes_ok: instance of NodeSet that contains the nodes correctly deployed
  # * nodes_ko: instance of NodeSet that contains the nodes not correctly deployed
  # Output
  # * nothing    
  def generate_files(nodes_ok, nodes_ko)
    File.delete("nodes_ok") if File.exist?("nodes_ok")
    File.delete("nodes_ko") if File.exist?("nodes_ko")
    t = nodes_ok.make_array_of_hostname
    if (not t.empty?) then
      file = File.new("nodes_ok", "w")
      t.each { |n|
        file.write("#{n}\n")
      }
      file.close
    end
    t = nodes_ko.make_array_of_hostname
    if (not t.empty?) then
      file = File.new("nodes_ko", "w")
      t.each { |n|
        file.write("#{n}\n")
      }
      file.close
    end
  end
end

def _exit(exit_code, dbh)
  dbh.disconnect
  exit(exit_code)
end


client_config = ConfigInformation::Config.load_client_config_file

#Connect to the server
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
common_config = kadeploy_server.get_common_config

#Connect to the database
db = Database::DbFactory.create(common_config.db_kind)
db.connect(common_config.deploy_db_host,
           common_config.deploy_db_login,
           common_config.deploy_db_passwd,
           common_config.deploy_db_name)

exec_specific_config = ConfigInformation::Config.load_kadeploy_exec_specific(common_config.nodes_desc, db)
if (exec_specific_config != nil) then
  #Rights check
  allowed_to_deploy = true
  #The rights must be checked for each cluster if the node_list contains nodes from several clusters
  exec_specific_config.node_list.group_by_cluster.each_pair { |cluster, set|
    if (exec_specific_config.deploy_part != "") then 
      part = exec_specific_config.deploy_part
    else
      part = kadeploy_server.get_default_deploy_part(cluster)
    end
    allowed_to_deploy = CheckRights::CheckRightsFactory.create(common_config.rights_kind,
                                                               set,
                                                               db,
                                                               part).granted?
  }

  if (allowed_to_deploy == true) then
    #Launch the listener on the client
    kadeploy_client = KadeployClient.new(kadeploy_server)
    DRb.start_service(nil, kadeploy_client)
    if /druby:\/\/([\w+.+]+):(\w+)/ =~ DRb.uri
      content = Regexp.last_match
      client_host = content[1]
      client_port= content[2]
    else
      puts "The URI #{DRb.uri} is not correct"
      _exit(1, db)
    end
    
    if (exec_specific_config != nil) then
      if (exec_specific_config.pxe_profile_file != "") then
        IO.readlines(exec_specific_config.pxe_profile_file).each { |l|
          exec_specific_config.pxe_profile_msg.concat(l)
        }
      end
      kadeploy_server.launch_workflow(client_host, client_port, exec_specific_config)
      #We execute a script at the end of the deployment if required
      if (exec_specific_config.script != "") then
        system(exec_specific_config.script)
      end
    end
  else
    puts "You do not have the deployment rights on all the nodes"
    _exit(1, db)
  end
  _exit(0, db)
else
  _exit(1, db)
end
