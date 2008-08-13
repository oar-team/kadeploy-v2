#!/usr/bin/ruby -w
require 'lib/config'
require 'lib/db'
require 'lib/environment'


def list_environments(config, db)
  env = EnvironmentManagement::Environment.new
  if (config.exec_specific.show_all_version == false) then
    query = "SELECT * FROM environment WHERE user=\"#{config.exec_specific.user}\" \
                                       AND version=(SELECT MAX(version) FROM environment WHERE user=\"#{config.exec_specific.user}\")"
  else
    query = "SELECT * FROM environment WHERE user=\"#{config.exec_specific.user}\"
                                       ORDER BY version"
  end
  res = db.run_query(query)
  env.short_view_header
  res.each_hash { |row|
    env.load_from_hash(row)
    env.short_view
  }
end

def add_environment(config, db)
  env = EnvironmentManagement::Environment.new
  env.load_from_file(config.exec_specific.file)
  query = "INSERT INTO environment (name, \
                                    version, \
                                    description, \
                                    author, \
                                    tarball_file, \
                                    tarball_md5, \
                                    postinstall_file, \
                                    postinstall_md5, \
                                    kernel, \
                                    kernel_params, \
                                    initrd, \
                                    part, \
                                    fdisk_type, \
                                    filesystem,user) \
                            VALUES (\"#{env.name}\", \
                                    \"#{env.version}\", \
                                    \"#{env.description}\", \
                                    \"#{env.author}\", \
                                    \"#{env.tarball_file}\", \
                                    \"#{env.tarball_md5}\", \
                                    \"#{env.postinstall_file}\", \
                                    \"#{env.postinstall_md5}\", \
                                    \"#{env.kernel}\", \
                                    \"#{env.kernel_params}\", \
                                    \"#{env.initrd}\", \
                                    \"#{env.part}\", \
                                    \"#{env.fdisk_type}\", \
                                    \"#{env.filesystem}\", \
                                    \"#{env.user}\")"
  db.run_query(query)
end

def delete_environment(config,db)
  env = EnvironmentManagement::Environment.new
  query = "DELETE FROM environment WHERE name=\"#{config.exec_specific.env_name}\" \
                                   AND user=\"#{ENV['USER']}\""
  db.run_query(query)
end

def print_environment(config, db)
  env = EnvironmentManagement::Environment.new
  if (config.exec_specific.show_all_version == false) then
    query = "SELECT * FROM environment WHERE name=\"#{config.exec_specific.env_name}\" \
                                       AND user=\"#{config.exec_specific.user}\" \
                                       AND version=(SELECT MAX(version) FROM environment WHERE user=\"#{config.exec_specific.user}\")"
  else
    query = "SELECT * FROM environment WHERE name=\"#{config.exec_specific.env_name}\" \
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

config = ConfigInformation::Config.new("kaenv")
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
  db.disconnect
end
