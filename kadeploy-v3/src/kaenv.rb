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
# * print the environments of a given user
def list_environments(config, db)
  env = EnvironmentManagement::Environment.new
  if (config.exec_specific.user == "*") then #we show the environments of all the users
    if (config.exec_specific.show_all_version == false) then
      query = "SELECT name, MAX(version) AS version, description, author, tarball, \
                    postinstall, kernel, kernel_params, \
                    initrd, hypervisor, hypervisor_params, part, fdisk_type, filesystem, user, environment_kind, demolishing_env \
                    FROM environments \
                    GROUP BY name"
    else
      query = "SELECT * FROM environments ORDER BY name,version"
    end
  else
    if (config.exec_specific.show_all_version == false) then
      query = "SELECT name, MAX(version) AS version, description, author, tarball, \
                      postinstall, kernel, kernel_params, \
                      initrd, hypervisor, hypervisor_params, part, fdisk_type, filesystem, user, environment_kind, demolishing_env \
                      FROM environments \
                      WHERE user=\"#{config.exec_specific.user}\" \
                      GROUP BY user,name"
    else
      query = "SELECT * FROM environments WHERE (user=\"#{config.exec_specific.user}\" \
                                          ORDER BY user,name,version"
    end
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
  if env.load_from_file(config.exec_specific.file) then
    query = "SELECT * FROM environments WHERE name=\"#{env.name}\" AND version=\"#{env.version}\" AND user=\"#{env.user}\""
    res = db.run_query(query)
    if (res.num_rows != 0) then
      puts "An environment with the name #{env.name} and the version #{env.version} has already been recorded for the user #{env.user}"
    else
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
                                         \"#{env.flatten_tarball_with_md5()}\", \
                                         \"#{env.flatten_post_install_with_md5()}\", \
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
  end
end

# Delete the environment specified in Config.exec_specific.env_name
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * nothing
def delete_environment(config, db)
  if (config.exec_specific.version != "") then
    version = config.exec_specific.version
  else
    version = get_max_version(db, config.exec_specific.env_name, ENV['USER'])
  end
  query = "DELETE FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                    AND version=\"#{version}\" \
                                    AND user=\"#{ENV['USER']}\"" #using $USER is not really good ...
  db.run_query(query)
end

# Print the environment designed by Config.exec_specific.env_name and that belongs to the user specified in Config.exec_specific.user
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * print the specified environment that belongs to the specified user
def print_environment(config, db)
  env = EnvironmentManagement::Environment.new
  if (config.exec_specific.show_all_version == false) then
    if (config.exec_specific.version != "") then
      version = config.exec_specific.version
    else
      version = get_max_version(db, config.exec_specific.env_name, config.exec_specific.user)
    end
    query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                        AND user=\"#{config.exec_specific.user}\" \
                                        AND version=\"#{version}\""
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

# Get the highest version of an environment
#
# Arguments
# * db: database handler
# * env_name: environment name
# * user: owner of the environment
# Output
# * return the highest version number or -1 if no environment is found
def get_max_version(db, env_name, user)
  query = "SELECT MAX(version) FROM environments WHERE user=\"#{user}\" \
                                                 AND name=\"#{env_name}\""
  res = db.run_query(query)
  if res != nil then
    row = res.fetch_row
    return row[0]
  else
    return -1
  end
end


# Update the md5sum of the tarball
#
# * config: instance of Config
# * db: database handler
# Output
# * nothing
def update_tarball_md5(config, db)
  if (config.exec_specific.version != "") then
    version = config.exec_specific.version
  else
    version = get_max_version(db, config.exec_specific.env_name, ENV['USER'])
  end
  query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                      AND user=\"#{ENV['USER']}\" \
                                      AND version=\"#{version}\""
  res = db.run_query(query)
  res.each_hash  { |row|
    env = EnvironmentManagement::Environment.new
    env.load_from_hash(row)
    tarball = "#{env.tarball["file"]}|#{env.tarball["kind"]}|#{env.get_md5(env.tarball["file"])}"
    
    query2 = "UPDATE environments SET tarball=\"#{tarball}\" WHERE name=\"#{config.exec_specific.env_name}\" \
                                                             AND user=\"#{ENV['USER']}\" \
                                                             AND version=\"#{version}\""
    db.run_query(query2)
  }
end

# Update the md5sum of the postinstall files
#
# * config: instance of Config
# * db: database handler
# Output
# * nothing
def update_postinstalls_md5(config, db)
  if (config.exec_specific.version != "") then
    version = config.exec_specific.version
  else
    version = get_max_version(db, config.exec_specific.env_name, ENV['USER'])
  end
  query = "SELECT * FROM environments WHERE name=\"#{config.exec_specific.env_name}\" \
                                      AND user=\"#{ENV['USER']}\" \
                                      AND version=\"#{version}\""
  res = db.run_query(query)
  res.each_hash  { |row|
    env = EnvironmentManagement::Environment.new
    env.load_from_hash(row)
    if (env.postinstall != nil) then
      s = String.new
      env.postinstall.each_index { |i|
        s += "#{env.postinstall[i]["file"]}|#{env.postinstall[i]["kind"]}|#{env.get_md5(env.postinstall[i]["file"])}|#{env.postinstall[i]["script"]}"
        s += "," if (i < env.postinstall.length - 1)
      }
      query2 = "UPDATE environments SET postinstall=\"#{s}\" WHERE name=\"#{config.exec_specific.env_name}\" \
                                                             AND user=\"#{ENV['USER']}\" \
                                                             AND version=\"#{version}\""
      db.run_query(query2)
    end
  }
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
    version = get_max_version(db, config.exec_specific.env_name, ENV['USER'])
  end
  query = "UPDATE environments SET demolishing_env=0 WHERE name=\"#{config.exec_specific.env_name}\" \
                                                     AND user=\"#{ENV['USER']}\" \
                                                     AND version=\"#{version}\""
  db.run_query(query)
end

def _exit(exit_code, dbh)
  dbh.disconnect if (dbh != nil)
  exit(exit_code)
end

begin
  config = ConfigInformation::Config.new("kaenv")
rescue
  _exit(1, nil)
end

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
    delete_environment(config, db)
  when "print"
    print_environment(config, db)
  when "update-tarball-md5"
    update_tarball_md5(config, db)
  when "update-postinstalls-md5"
    update_postinstalls_md5(config, db)
  when "remove-demolishing-tag"
    remove_demolishing_tag(config, db)
  end
  _exit(0, db)
else
  _exit(1, db)
end
