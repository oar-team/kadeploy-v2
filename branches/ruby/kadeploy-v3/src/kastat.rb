#!/usr/bin/ruby -w

#Kadeploy libs
require 'config'
require 'db'

#Ruby libs
require 'drb'

# Generate some filters for the output according the options
#
# Arguments
# * config: instance of Config
# Output
# * returns a string that contains the where clause corresponding to the filters required
def append_generic_where_clause(config)
  generic_where_clause = String.new
  node_list = String.new
  date_min = String.new
  date_max = String.new
  if (not config.exec_specific.node_list.empty?) then
    node_list = "("
    config.exec_specific.node_list.each_index { |i|
      node_list += "hostname=\"#{config.exec_specific.node_list[i]}\""
      if (i < config.exec_specific.node_list.length - 1) then
        node_list += " OR "
      else
        node_list += ")"
      end
    }
  end
  if (config.exec_specific.date_min != 0) then
    date_min = "start>=\"#{config.exec_specific.date_min}\""
  end
  if (config.exec_specific.date_max != 0) then
    date_max = "start<=\"#{config.exec_specific.date_max}\""
  end
  if ((node_list != "") || (date_min != "") || (date_max !="")) then
    generic_where_clause = "#{node_list} AND #{date_min} AND #{date_max}"
    #let's clean empty things
    generic_where_clause = generic_where_clause.gsub("AND  AND","")
    generic_where_clause = generic_where_clause.gsub(/^ AND/,"")
    generic_where_clause = generic_where_clause.gsub(/AND $/,"")
  end
  return generic_where_clause
end


# Select the fields to output
#
# Arguments
# * row: hashtable that contains a line of information fetched in the database
# * config: instance of Config
# * default_fields: array of fields used to produce the output if no fields are given in the command line
# Output
# * string that contains the selected fields in a result line
def select_fields(row, config, default_fields)
  fields = String.new  
  if (not config.exec_specific.fields.empty?) then
    config.exec_specific.fields.each_index { |i|
      fields += row[config.exec_specific.fields[i]].gsub("\n", "\\n")
      if (i < config.exec_specific.fields.length - 1) then
        fields += ","
      end    
    }
  else
    default_fields.each_index { |i|
      fields += row[default_fields[i]].gsub("\n", "\\n")
      if (i < default_fields.length - 1) then
        fields += ","
      end
    }
  end
  return fields
end

# List the information about the nodes that require a given number of retries to be deployed
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * prints the filtred information about the nodes that require a given number of retries to be deployed
def list_retries(config, db)
  step_list = String.new
  if (not config.exec_specific.steps.empty?) then
    config.exec_specific.steps.each_index { |i|
      case config.steps[i]
      when "1"
        step_list += "retry_step1>=\"#{config.exec_specific.min_retries}\""
      when "2"
        step_list += "retry_step2>=\"#{config.exec_specific.min_retries}\""
      when "3"
        step_list += "retry_step3>=\"#{config.exec_specific.min_retries}\""
      end
      if (i < config.exec_specific.steps.length - 1) then
        step_list += " AND "
      end
    }
  else
    step_list += "(retry_step1>=\"#{config.exec_specific.min_retries}\""
    step_list += " OR retry_step2>=\"#{config.exec_specific.min_retries}\""
    step_list += " OR retry_step3>=\"#{config.exec_specific.min_retries}\")"
  end

  generic_where_clause = append_generic_where_clause(config)
  if (generic_where_clause == "") then
    query = "SELECT * FROM log WHERE #{generic_where_clause}"
  else
    query = "SELECT * FROM log WHERE #{generic_where_clause} AND #{step_list}"
  end
  res = db.run_query(query)
  res.each_hash { |row|
    puts select_fields(row, config, ["start","hostname","retry_step1","retry_step2","retry_step3"])
  }
end


# List the information about the nodes that have at least a given failure rate
#
# Arguments
# * config: instance of Config
# * db: database handler
# * min(opt): minimum failure rate
# Output
# * prints the filtred information about the nodes that have at least a given failure rate
def list_failure_rate(config, db, min = nil)
  generic_where_clause = append_generic_where_clause(config)
  if (generic_where_clause != "") then
    query = "SELECT * FROM log WHERE #{generic_where_clause}"
  else
    query = "SELECT * FROM log"
  end
  res = db.run_query(query)
  hash = Hash.new
  res.each_hash { |row|
    if (not hash.has_key?(row["hostname"])) then
      hash[row["hostname"]] = Array.new
    end
    hash[row["hostname"]].push(row["success"])
  }
  hash.each_pair { |hostname, array|
    success = 0
    array.each { |val|
      if (val == "true") then
        success += 1
      end
    }
    rate = 100 - (100 * success / array.length)
    if ((min == nil) || (rate >= min)) then
      puts "#{hostname}: #{rate}%"
    end
  }
end

# List the information about all the nodes
#
# Arguments
# * config: instance of Config
# * db: database handler
# Output
# * prints the information about all the nodes
def list_all(config, db)
  generic_where_clause = append_generic_where_clause(config)
  if (generic_where_clause != "") then
    query = "SELECT * FROM log WHERE #{generic_where_clause}"
  else
    query = "SELECT * FROM log"
  end
  res = db.run_query(query)
  res.each_hash { |row|
    puts select_fields(row, config, ["user","hostname","step1","step2","step3", \
                                     "timeout_step1","timeout_step2","timeout_step3", \
                                     "retry_step1","retry_step2","retry_step3", \
                                     "start", \
                                     "step1_duration","step2_duration","step3_duration", \
                                     "env","md5", \
                                     "success","error"])
  } 
end

def _exit(exit_code, dbh)
  dbh.disconnect if (dbh != nil)
  exit(exit_code)
end



config = ConfigInformation::Config.new("kastat")

#Connect to the Kadeploy server to get the common configuration
client_config = ConfigInformation::Config.load_client_config_file
DRb.start_service()
uri = "druby://#{client_config.kadeploy_server}:#{client_config.kadeploy_server_port}"
kadeploy_server = DRbObject.new(nil, uri)
config.common = kadeploy_server.get_common_config

if (config.check_config("kastat") == true) then
  db = Database::DbFactory.create(config.common.db_kind)
  db.connect(config.common.deploy_db_host,
             config.common.deploy_db_login,
             config.common.deploy_db_passwd,
             config.common.deploy_db_name)

  case config.exec_specific.operation
  when "list_all"
    list_all(config, db)
  when "list_retries"
    list_retries(config, db)
  when "list_failure_rate"
    list_failure_rate(config, db)
  when "list_min_failure_rate"
    list_failure_rate(config, db, config.exec_specific.min_rate)
  end
  _exit(0, db)
else
  _exit(1, db)
end
