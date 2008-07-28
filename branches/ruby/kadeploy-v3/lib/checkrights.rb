 module CheckRights

  class CheckRightsFactory
    attr_accessor :klass

    def CheckRightsFactory.create(kind)
      case kind
      when /dummy/
        return CheckDummy.new
      when /db/
        return CheckInDB.new
      else
        raise "Invalid kind of rigths check"
      end
    end
  end


  class Check
    @granted = false
    
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
    def initialize
      puts "todo"
      @granted = false
    end
  end

end
