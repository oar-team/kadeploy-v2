#Ruby libs
require 'mysql'

module Database
  class DbFactory

    # Factory for the methods to access the database
    #
    # Arguments
    # * kind: specifies the kind of database to use (currently, only mysql is supported)
    # Output
    # * returns a Db instance (DbMysql)
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

    #Constructor of Db
    #
    # Arguments
    # * nothing
    # Output
    # * nothing    
    def initialize
      @dbh = nil
    end

    # Abstract method to connect to the database
    #
    # Arguments
    # * host: hostname
    # * user: user granted to access the database
    # * passwd: user's password
    # * base: database name
    # Output
    # * nothing
    def connect(host, user, passwd, base)
    end

    # Abstract method to disconnects from the database
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def disconnect
    end

    # Abstract method to run a query
    #
    # Arguments
    # * query: string that contains the sql query
    # Output
    # * nothing
    def run_query(query)
    end
  end

  class DbMysql < Db

    # Connect to the MySQL database
    #
    # Arguments
    # * host: hostname
    # * user: user granted to access the database
    # * passwd: user's password
    # * base: database name
    # Output
    # * prints an error if the connection can not be performed, otherwhise assigns a database handler to @dhb
    def connect(host, user, passwd, base)
      @dbh = Mysql.real_connect(host, user, passwd, base)
    rescue Mysql::Error => e
      puts "Error code: #{e.errno}"
      puts "Error message: #{e.error}"
    end

    # Disconnect from the MySQL database
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def disconnect
      @dbh.close
    end

    # Run a query
    #
    # Arguments
    # * query: string that contains the sql query
    # Output
    # * returns a MySQL::result and print an error if the execution failed
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
