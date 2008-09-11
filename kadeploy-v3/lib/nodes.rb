module Nodes
  class NodeCmd
    attr_accessor :reboot_soft
    attr_accessor :reboot_hard
    attr_accessor :reboot_very_hard
    attr_accessor :console
  end

  class Node
    attr_accessor :hostname   #fqdn
    attr_accessor :ip         #aaa.bbb.ccc.ddd
    attr_accessor :cluster
    attr_accessor :state      #OK,KO
    attr_accessor :current_step
    attr_accessor :last_cmd_exit_status
    attr_accessor :last_cmd_stdout
    attr_accessor :last_cmd_stderr
    attr_accessor :cmd

    # Constructor of Node
    #
    # Arguments
    # * hostname: name of the host
    # * ip: ip of the host
    # * cluster: name of the cluster
    # * cmd: instance of NodeCmd
    # Output
    # * nothing
    def initialize(hostname, ip, cluster, cmd)
      @hostname=hostname
      @ip=ip
      @cluster = cluster
      @state="OK"
      @cmd = cmd
    end
    
    # Make a string with the characteristics of a node
    #
    # Arguments
    # * dbg(opt): boolean that specifies if the output contains stderr
    # Output
    # * returns a string that contains the information
    def to_s(dbg = false)
      if (dbg) && (last_cmd_stderr != nil) then
        return "#{@hostname} (#{@last_cmd_stderr})"
      else
        return "#{@hostname}"
      end
    end
  end

  class NodeSet
    attr_accessor :set
    
    # Constructor of NodeSet
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def initialize
      @set = Array.new
    end

    # Adds a node to the set
    #
    # Arguments
    # * node: instance of Node
    # Output
    # * nothing
    def push(node)
      @set.push(node)
    end

    # Tests if the node set is empty
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the set is empty, false otherwise
    def empty?
      return @set.empty?
    end

    # Duplicate a NodeSet
    #
    # Arguments
    # * dest: destination NodeSet
    # Output
    # * nothing
    def duplicate(dest)
      @set.each { |node|
        dest.push(node.clone)
      }
    end

    # Duplicate a NodeSet and free it
    #
    # Arguments
    # * dest: destination NodeSet
    # Output
    # * nothing
    def duplicate_and_free(dest)
      @set.each { |node|
        dest.push(node.clone)
      }
      free
    end

    # Adds a NodeSet to an existing one
    #
    # Arguments
    # * node_set: NodeSet to add to the current one
    # Output
    # * nothing
    def add(node_set)
      if not node_set.empty?
        node_set.set.each { |node|
          @set.push(node)
        }
      end
    end

    # Tests if the state of all the nodes in the set is OK
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the state of all the nodes is OK     
    def all_ok?
      @set.each { |node|
        if node.state == "KO"
          return false
        end
      }
      return true
    end

    # Free a NodeSet
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def free
      @set.delete_if { |node| true }
    end

    # Creates an array from the IP of the nodes in a NodeSet
    #
    # Arguments
    # * nothing
    # Output
    # * returns an array of IP
    def make_array_of_ip
      res = Array.new
      @set.each { |n|
        res.push(n.ip)
      }
      return res
    end

    # Creates an array from the hostname of the nodes in a NodeSet
    #
    # Arguments
    # * nothing
    # Output
    # * returns an array of hostname
    def make_array_of_hostname
      res = Array.new
      @set.each { |n|
        res.push(n.hostname)
      }
      return res
    end

    # Make a string with the characteristics of the nodes of a NodeSet
    #
    # Arguments
    # * nothing
    # Output
    # * returns a string that contains the information
    def to_s(dbg = false)
      s = String.new
      @set.each_index { |i|
        s += @set[i].to_s(dbg)
        s += ", " if (i < @set.length - 1)
      }
      return s
    end

    # Make a hashtable that groups the nodes in a NodeSet by cluster
    #
    # Arguments
    # * nothing
    # Output
    # * returns an Hash that groups the nodes by cluster (each entry is a NodeSet)
    def group_by_cluster
      ht = Hash.new
      @set.each { |node| 
        ht[node.cluster] = NodeSet.new if ht[node.cluster].nil?
        ht[node.cluster].push(node.clone)
      }
      return ht
    end

    # Gets a Node in a NodeSet by its hostname
    #
    # Arguments
    # * hostname: name of the node searched
    # Output
    # * returns nil if the node can not be found
    def get_node_by_host(hostname)
      @set.each { |node|
        return node if (node.hostname == hostname)
      }
      return nil
    end

    # Set an error message to a NodeSet
    #
    # Arguments
    # * msg: error message
    # Output
    # * nothing
    def set_error_msg(msg)
      @set.each { |node|
        node.last_cmd_stderr = msg
      }
    end
  end
end
