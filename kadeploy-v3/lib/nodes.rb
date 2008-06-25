module Nodes
  class Node
    attr_accessor :hostname   #fqdn
    attr_accessor :ip         #aaa.bbb.ccc.ddd
    attr_accessor :cluster
    attr_accessor :state      #OK,KO
    attr_accessor :current_step
    attr_accessor :last_cmd_exit_status
    attr_accessor :last_cmd_stdout
    attr_accessor :last_cmd_stderr
    
    def initialize(hostname, ip, cluster)
      @hostname=hostname
      @ip=ip
      @cluster = cluster
      @state="OK"
    end
    
    def to_s(dbg = false)
      if (dbg) then
        return "#{@hostname} (#{@last_cmd_stderr})"
      else
        return "#{@hostname}"
      end
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

    def add(node_set)
      if not node_set.empty?
        node_set.set.each { |node|
          @set.push(node)
        }
      end
    end
      
    def all_ok?
      @set.each { |node|
        if node.state == "KO"
          return false
        end
      }
      return true
    end

    def free
      @set.delete_if { |node| true }
    end

    def make_array_of_ip
      res = Array.new
      @set.each { |n|
        res.push(n.ip)
      }
      return res
    end

    def to_s(dbg = false)
      s = String.new
      @set.each_index { |i|
        s += @set[i].to_s(dbg)
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

    def get_node_by_host(hostname)
      @set.each { |node|
        return node if (node.hostname == hostname)
      }
      return nil
    end
  end

  class NodeCmd
    attr_accessor :reboot_soft
    attr_accessor :reboot_hard
    attr_accessor :reboot_very_hard
    attr_accessor :console
  end
end
