#!/usr/bin/ruby -w
require 'lib/config'
require 'lib/db'



config = ConfigInformation::Config.new("kaenv")
if (config.check_config("kaenv") == true)
  db = Database::DbFactory.create(config.common.db_kind)
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)
  res = db.run_query("SELECT * from environment")

  db.disconnect
end
