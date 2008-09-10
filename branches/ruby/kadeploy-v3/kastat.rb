#!/usr/bin/ruby -w
require 'lib/config'
require 'lib/db'


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

def select_fields(row, config, default_fields)
  fields = String.new  
  if (not config.exec_specific.fields.empty?) then
    config.exec_specific.fields.each_index { |i|
      fields += row[config.exec_specific.fields[i]]
      if (i < config.exec_specific.fields.length - 1) then
        fields += ","
      end    
    }
  else
    default_fields.each_index { |i|
      fields += row[default_fields[i]]
      if (i < default_fields.length - 1) then
        fields += ","
      end
    }
  end
  return fields
end

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
      puts select_fields(row, config, ["hostname","rate"])
    end
  }
end

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

config = ConfigInformation::Config.new("kastat")
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
  db.disconnect
end
