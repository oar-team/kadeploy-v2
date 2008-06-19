module Nodes
  class Node
    attr_accessor :hostname   #fqdn
    attr_accessor :ip         #aaa.bbb.ccc.ddd
    attr_accessor :cluster
    attr_accessor :state      #OK,KO
    attr_accessor :current_step
    attr_accessor :last_error_msg

    def initialize(hostname, ip, cluster)
      @hostname=hostname
      @ip=ip
      @cluster = cluster
      @state="OK"
    end
    
    def to_s
      return "#{@hostname}|#{@ip}"
    end
  end

  class NodeSet
    attr_accessor :set
    
    def initialize
      @set = Array.new
    end

    def push(node)
      @set.push(node)
    end
       
    def empty?
      return @set.empty?
    end

    def duplicate(dest)
      @set.each { |node|
        dest.push(node.clone)
      }
    end

    def duplicate_and_free(dest)
      @set.each { |node|
        dest.push(node.clone)
      }
      free
    end
      
    def free
      @set.delete_if { |node| true }
    end

    def to_s
      s = String.new
      @set.each_index { |i|
        s += @set[i].to_s
        s += ", " if (i < @set.length - 1)
      }
      return s
    end
    
    #group the nodes by cluster and return an hashtable (each entry is an array of nodes)
    def group_by_cluster
      ht = Hash.new
      @set.each { |node| 
        ht[node.cluster] = NodeSet.new if ht[node.cluster].nil?
        ht[node.cluster].push(node.clone)
      }
      return ht
    end
  end
end
