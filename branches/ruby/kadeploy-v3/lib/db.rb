require "mysql"

module Database
  class DbFactory
    def DbFactory.create(kind)
      case kind
      when "mysql"
        return DbMysql.new
      else
        raise "Invalid kind of database"
      end
    end
  end
  
  class Db
    attr_accessor :dbh

    def initialize
      @dbh = nil
    end

    def connect(host, user, passwd, base)
    end

    def disconnect
    end

    def run_query(query)
    end
  end

  class DbMysql < Db
    def connect(host, user, passwd, base)
      @dbh = Mysql.real_connect(host, user, passwd, base)
    rescue Mysql::Error => e
      puts "Error code: #{e.errno}"
      puts "Error message: #{e.error}"
    end

    def disconnect
      @dbh.close
    end
    
    def run_query(query)
      res = nil
      begin
        res = @dbh.query(query)
      rescue Mysql::Error => e
        puts "Error code: #{e.errno}"
        puts "Error message: #{e.error}"
      end
      return res
    end
  
  end
end
