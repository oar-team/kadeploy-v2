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
    query = "SELECT name, MAX(version) AS version, description, author, tarball, \
                    postinstall, kernel, kernel_params, \
                    initrd, hypervisor, hypervisor_params, part, fdisk_type, filesystem, user, environment_kind, demolishing_env \
                    FROM environments \
                    WHERE user=\"#{config.exec_specific.user}\" \
                    GROUP BY name"
  else
    query = "SELECT * FROM environments WHERE user=\"#{config.exec_specific.user}\"
                                        ORDER BY version"
  end
  res = db.run_query(query)
  env.short_view_header
  if (res != nil) then
    res.each_hash { |row|
      env.load_from_hash(row)
      env.short_view
    }
  end
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
  env.load_from_file(config.exec_specific.file, config.exec_specific.check_md5)
  query = "INSERT INTO environments (name, \
                                    version, \
                                    description, \
                                    author, \
                                    tarball, \
                                    postinstall, \
                                    kernel, \
                                    kernel_params, \
                                    initrd, \
                                    hypervisor, \
                                    hypervisor_params, \
                                    part, \
                                    fdisk_type, \
                                    filesystem, \
                                    user, \
                                    environment_kind,
                                    demolishing_env) \
                            VALUES (\"#{env.name}\", \
                                    \"#{env.version}\", \
                                    \"#{env.description}\", \
                                    \"#{env.author}\", \
                                    \"#{env.flatten_tarball()}\", \
                                    \"#{env.flatten_post_install()}\", \
                                    \"#{env.kernel}\", \
                                    \"#{env.kernel_params}\", \
                                    \"#{env.initrd}\", \
                                    \"#{env.hypervisor}\", \
                                    \"#{env.hypervisor_params}\", \
                                    \"#{env.part}\", \
                                    \"#{env.fdisk_type}\", \
                                    \"#{env.filesystem}\", \
                                    \"#{env.user}\", \
                                    \"#{env.environment_kind}\", \
                                    \"#{env.demolishing_env}\")"
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
    if (config.exec_specific.version != "") then
      query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                          AND user=\"#{config.exec_specific.user}\" \
                                          AND version=\"#{config.exec_specific.version}\""
    else
      query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                          AND user=\"#{config.exec_specific.user}\" \
                                          AND version=(SELECT MAX(version) FROM environments \
                                                                           WHERE user=\"#{config.exec_specific.user}\" \
                                                                           AND name=\"#{config.exec_specific.env_name}\")"
    end
  else
    query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                        AND user=\"#{config.exec_specific.user}\" \
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

#Remove the demolishing tag on an environment
#
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * nothing
def remove_demolishing_tag(config, db)
  #if no version number is given, we only remove the demolishing tag on the last version
  if (config.exec_specific.version != "") then
    version = config.exec_specific.version
  else
    query = "SELECT MAX(version) FROM environments WHERE user=\"#{config.exec_specific.user}\" \
                                                   AND name=\"#{config.exec_specific.env_name}\""
    res = db.run_query(query)
    row = res.fetch_row
    version = row[0]
  end
  query = "UPDATE environments SET demolishing_env=0 WHERE name=\"#{config.exec_specific.env_name}\" \
                                                     AND user=\"#{config.exec_specific.user}\" \
                                                     AND version=\"#{version}\""
  db.run_query(query)
end

def _exit(exit_code, dbh)
  dbh.disconnect if (dbh != nil)
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
  when "remove-demolishing-tag"
    remove_demolishing_tag(config, db)
  end
  _exit(0, db)
else
  _exit(1, db)
end
