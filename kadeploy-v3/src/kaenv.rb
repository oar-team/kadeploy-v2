#!/usr/bin/ruby -w

#Kadeploy libs
require 'config'
require 'db'
require 'environment'

#Ruby libs
require 'drb'

# List the environments of a user defined in Config.exec_specific.user
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * prints the environments of a given user
def list_environments(config, db)
  env = EnvironmentManagement::Environment.new
  if (config.exec_specific.show_all_version == false) then
    query = "SELECT * FROM environments WHERE user=\"#{config.exec_specific.user}\" \
                                        AND version=(SELECT MAX(version) FROM environments WHERE user=\"#{config.exec_specific.user}\")"
  else
    query = "SELECT * FROM environments WHERE user=\"#{config.exec_specific.user}\"
                                        ORDER BY version"
  end
  res = db.run_query(query)
  env.short_view_header
  res.each_hash { |row|
    env.load_from_hash(row)
    env.short_view
  }
end

# Add an environment described in the file Config.exec_specific.file
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * nothing
def add_environment(config, db)
  env = EnvironmentManagement::Environment.new
  env.load_from_file(config.exec_specific.file)
  query = "INSERT INTO environments (name, \
                                    version, \
                                    description, \
                                    author, \
                                    tarball_file, \
                                    tarball_kind, \
                                    tarball_md5, \
                                    postinstall_file, \
                                    postinstall_kind, \
                                    postinstall_md5, \
                                    kernel, \
                                    kernel_params, \
                                    initrd, \
                                    part, \
                                    fdisk_type, \
                                    filesystem, \
                                    user, \
                                    environment_kind) \
                            VALUES (\"#{env.name}\", \
                                    \"#{env.version}\", \
                                    \"#{env.description}\", \
                                    \"#{env.author}\", \
                                    \"#{env.tarball_file}\", \
                                    \"#{env.tarball_kind}\", \
                                    \"#{env.tarball_md5}\", \
                                    \"#{env.postinstall_file}\", \
                                    \"#{env.postinstall_kind}\", \
                                    \"#{env.postinstall_md5}\", \
                                    \"#{env.kernel}\", \
                                    \"#{env.kernel_params}\", \
                                    \"#{env.initrd}\", \
                                    \"#{env.part}\", \
                                    \"#{env.fdisk_type}\", \
                                    \"#{env.filesystem}\", \
                                    \"#{env.user}\", \
                                    \"#{env.environment_kind}\")"
  db.run_query(query)
end

# Delete the environment specified in Config.exec_specific.env_name
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * nothing
def delete_environment(config, db)
  env = EnvironmentManagement::Environment.new
  query = "DELETE FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                    AND user=\"#{ENV['USER']}\""
  db.run_query(query)
end

# Print the environment designed by Config.exec_specific.env_name and that belongs to the user specified in Config.exec_specific.user
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * prints the specified environment that belongs to the specified user
def print_environment(config, db)
  env = EnvironmentManagement::Environment.new
  if (config.exec_specific.show_all_version == false) then
    query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                        AND user=\"#{config.exec_specific.user}\" \
                                        AND version=(SELECT MAX(version) FROM environments WHERE user=\"#{config.exec_specific.user}\")"
  else
    query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                        AND user=\"#{config.exec_specific.user}\"
                                        ORDER BY version"
  end
  res = db.run_query(query)
  if res != nil then
    res.each_hash { |row|
      puts "###"
      env.load_from_hash(row)
      env.full_view
    }
  end
end

def _exit(exit_code, dbh)
  dbh.disconnect
  exit(exit_code)
end

config = ConfigInformation::Config.new("kaenv")

#Connect to the Kadeploy server to get the common configuration
client_config = ConfigInformation::Config.load_client_config_file
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
config.common = kadeploy_server.get_common_config

if (config.check_config("kaenv") == true)
  db = Database::DbFactory.create(config.common.db_kind)
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)

  case config.exec_specific.operation  
  when "list"
    list_environments(config, db)
  when "add"
    add_environment(config, db)
  when "delete"
    delete_environment(config,db)
  when "print"
    print_environment(config, db)
  end
  _exit(0, db)
else
  _exit(1, db)
end
