module Debug
  class OutputControl
    @debug_level = 0
    
    def initialize(debug_level)
      @debug_level=debug_level
    end

    def debugl(l,msg)
      if (l <= @debug_level)
        puts msg
      end
    end
    
    def system_wrapper(l,cmd)
      puts "todo"
    end
  end
end
