module Debug
  class OutputControl
    @debug_level = 0
    @client = nil

    def initialize(debug_level, client)
      @debug_level=debug_level
      @client = client
    end

    def debugl(l, msg)
      if (l <= @debug_level)
        puts msg
        @client.print(msg)
      end
    end
    
    def system_wrapper(l,cmd)
      puts "todo"
    end
  end
end
