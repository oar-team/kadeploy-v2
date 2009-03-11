#Kadeploy libs
require 'db'

#Ruby libs
require 'digest/md5'

module EnvironmentManagement
  class Environment
    attr_reader :id
    attr_reader :name
    attr_reader :version
    attr_reader :description
    attr_reader :author
    attr_accessor :tarball
    attr_accessor :postinstall
    attr_reader :kernel
    attr_reader :kernel_params
    attr_reader :initrd
    attr_reader :hypervisor
    attr_reader :hypervisor_params
    attr_reader :part
    attr_reader :fdisk_type
    attr_reader :filesystem
    attr_reader :user
    attr_reader :environment_kind
    attr_reader :demolishing_env

    # Load an environment file
    #
    # Arguments
    # * file: filename
    # * check_md5: specify if the md5sum of the tarball and the post-install files must be checked
    # Output
    # * returns true if the environment can be loaded correctly, false otherwise
    # * raises an exception if the file does not exist
    def load_from_file(file, check_md5)
      if not File.exist?(file)
        put "#{file} does not exist"
        return false
      else
        @hypervisor = String.new
        @hypervisor_params = String.new
        IO::read(file).split("\n").each { |line|
          if /^(\w+)\ :\ (.+)/ =~ line then
            content = Regexp.last_match
            attr = content[1]
            val = content[2]
            case attr
            when "name"
              @name = val
            when "version"
              @version = val
            when "description"
              @description = val
            when "author"
              @author = val
            when "tarball"
              #filename|tgz|md5sum
              if val =~ /.+|\w+|\w+/ then
                @tarball = Hash.new
                tmp = val.split("|")
                @tarball["file"] = tmp[0]
                @tarball["kind"] = tmp[1]
                @tarball["md5"] = tmp[2]
              else
                puts "invalid entry for tarball"
                return false
              end
            when "postinstall"
              #filename|tgz|md5sum,filename|tgz|md5sum,...
              if val =~ /.+|\w+|\w+(,.+|\w+|\w+)*/ then
                @postinstall = Array.new
                val.split(",").each { |tmp|
                  tmp2 = tmp.split("|")
                  entry = Hash.new
                  entry["file"] = tmp2[0]
                  entry["kind"] = tmp2[1]
                  if ((entry["kind"] != "tgz") && (entry["kind"] != "tbz2")) then
                    puts "Only tgz and tbz2 file kinds are allowed for postinstall files"
                    return false
                  end
                  entry["md5"] = tmp2[2]
                  entry["script"] = tmp2[3]
                  @postinstall.push(entry)
                }
              else
                puts "invalid entry for postinstall"
                return false
              end
            when "kernel"
              @kernel = val
            when "kernel_params"
              @kernel_params = val
            when "initrd"
              @initrd = val
            when "hypervisor"
              @hypervisor = val
            when "hypervisor_params"
              @hypervisor_params = val
            when "part"
              @part = val
            when "fdisk_type"
              @fdisk_type = val
            when "filesystem"
              @filesystem = val
            when "user"
              @user = val
            when "environment_kind"
              if (val == "linux") || (val == "xen") || (val == "other") then
                @environment_kind = val
              else
                puts "#{val} is an invalid environment kind (linux and other are authorized)"
                return false
              end
            when "demolishing_env"
              @demolishing_env = val
            else
              puts "#{attr} is an invalid attribute"
              return false
            end
          end
        }
      end
      if check_md5 then
        return check_md5_digest
      else
        return true
      end
    end

    # Load an environment from a database
    #
    # Arguments
    # * name: environment name
    # * version: environment version
    # * user: environment owner
    # * dbh: database handler
    # Output
    # * returns true if the environment can be loaded, false otherwise
    def load_from_db(name, version, user, dbh)
      if (version == nil) then
        query = "SELECT * FROM environments WHERE name=\"#{name}\" \
                                             AND user=\"#{user}\" \
                                             AND version=(SELECT MAX(version) FROM environments WHERE user=\"#{user}\" AND name=\"#{name}\")"
      else
        query = "SELECT * FROM environments WHERE name=\"#{name}\" \
                                             AND user=\"#{user}\" \
                                             AND version=\"#{version}\""
      end
      res = dbh.run_query(query)
      row = res.fetch_hash
      if (row != nil) #We only take the first result since no other result should be returned
        load_from_hash(row)
        return true
      else
        puts "The environment #{name} cannot be loaded. Maybe the version number does not exist or it belongs to another user"
        return false
      end
    end

    # Load an environment from an Hash
    #
    # Arguments
    # * hash: hashtable
    # Output
    # * nothing
    def load_from_hash(hash)
      @id = hash["id"]
      @name = hash["name"]
      @version = hash["version"]
      @description = hash["description"]
      @author = hash["author"]
      @tarball = Hash.new
      val = hash["tarball"].split("|")
      @tarball["file"] = val[0]
      @tarball["kind"] = val[1]
      @tarball["md5"] = val[2]
      @postinstall = Array.new
      hash["postinstall"].split(",").each { |tmp|
        val = tmp.split("|")
        entry = Hash.new
        entry["file"] = val[0]
        entry["kind"] = val[1]
        entry["md5"] = val[2]
        entry["script"] = val[3]
        @postinstall.push(entry)
      }
      @kernel = hash["kernel"]
      @kernel_params = hash["kernel_params"]
      @initrd = hash["initrd"]
      @hypervisor = hash["hypervisor"]
      @hypervisor_params = hash["hypervisor_params"]
      @part = hash["part"]
      @fdisk_type = hash["fdisk_type"]
      @filesystem = hash["filesystem"]
      @user = hash["user"]
      @environment_kind = hash["environment_kind"]
      @demolishing_env = hash["demolishing_env"]
    end

    # Check the MD5 digest of the files
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the digest is OK, false otherwise
    def check_md5_digest
      val = @tarball.split("|")
      tarball_file = val[0]
      tarball_md5 = val[2]
      if (Digest::MD5.hexdigest(File.read(tarball_file)) != tarball_md5) then
        return false
      end
      @postinstall.split(",").each { |entry|
        val = entry.split("|")
        postinstall_file = val[0]
        postinstall_md5 = val[2]
        if (Digest::MD5.hexdigest(File.read(postinstall_file)) != postinstall_md5) then
          return false
        end       
      }
      return true
    end

    # Print the header
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def short_view_header
      puts "Name                Version     User            Description"
      puts "####                #######     ####            ###########"
    end

    # Print the short view
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def short_view
      printf("%-21s %-7s %-10s %-40s\n", @name, @version, @user, @description)
    end

    # Print the full view
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def full_view
      puts "name : #{@name}"
      puts "version : #{@version}"
      puts "description : #{@description}"
      puts "author : #{@author}"
      puts "tarball : #{flatten_tarball()}"
      puts "postinstall : #{flatten_post_install()}"
      puts "kernel : #{@kernel}"
      puts "kernel_params : #{@kernel_params}"
      puts "initrd : #{@initrd}"
      puts "hypervisor : #{@hypervisor}" if (@hypervisor != "")
      puts "hypervisor_params : #{@hypervisor_params}" if (@hypervisor_params != "")
      puts "part : #{@part}"
      puts "fdisk_type : #{@fdisk_type}"
      puts "filesystem : #{@filesystem}"
      puts "user : #{@user}"
      puts "environment_kind : #{@environment_kind}"
      puts "demolishing_env : #{@demolishing_env}"
    end

    def flatten_tarball
      return "#{@tarball["file"]}|#{@tarball["kind"]}|#{@tarball["md5"]}"
    end

    def flatten_post_install
      s = String.new
      @postinstall.each_index { |i|
        s += "#{@postinstall[i]["file"]}|#{@postinstall[i]["kind"]}|#{@postinstall[i]["md5"]}|#{@postinstall[i]["script"]}"
        s += "," if (i < @postinstall.length - 1)
      }
      return s
    end
  end
end
