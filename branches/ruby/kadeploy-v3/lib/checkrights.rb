 module CheckRights
  class CheckRightsFactory
    def CheckRightsFactory.create(kind, node_list = nil, db = nil, part = nil)
      case kind
      when "dummy"
        return CheckDummy.new
      when "db"
        return CheckInDB.new(node_list, db, part)
      else
        raise "Invalid kind of rights check"
      end
    end
  end

  class Check
    @granted = nil
    
    def initialize
      @granted = false
    end

    def granted?
      return @granted
    end
  end

  class CheckDummy < Check

    def initialize
      @granted = true
    end
  end

  class CheckInDB < Check
    @db = nil
    @host_list = nil
    @part = nil

    def initialize(node_list, db, part)
      @host_list = node_list.make_array_of_hostname
      @db = db
      @part = part
      @granted = false
    end

    def granted?
      query = "SELECT * FROM rights WHERE user=\"#{ENV['USER']}\" AND (part=\"#{@part}\" OR part=\"*\")"
      @host_list.each { |host|
        node_found = false
        res = @db.run_query(query)
        while ((hash = res.fetch_hash) && (node_found == false)) do
          if ((hash["node"] == host) || (hash["node"] == "*")) then
            node_found = true
          end
        end
        if (node_found == false) then
          puts "You do not have the rights to deploy on the node #{host}:#{@part}"
          return false
        end
      }
      return true
    end
  end
end
