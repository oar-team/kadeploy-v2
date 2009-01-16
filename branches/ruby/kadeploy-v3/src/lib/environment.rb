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
    attr_accessor :tarball_file
    attr_reader :tarball_kind
    attr_reader :tarball_md5
    attr_accessor :postinstall_file
    attr_reader :postinstall_kind
    attr_reader :postinstall_md5
    attr_reader :kernel
    attr_reader :kernel_params
    attr_reader :initrd
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
    # Output
    # * returns true if the environment can be loaded correctly, false otherwise
    # * raises an exception if the file does not exist
    def load_from_file(file)
      if not File.exist?(file)
        put "#{file} does not exist"
        return false
      else
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
            when "tarball_file"
              @tarball_file = val
            when "tarball_kind"
              @tarball_kind = val
            when "tarball_md5"
              @tarball_md5 = val
            when "postinstall_file"
              @postinstall_file = val
            when "postinstall_kind"
              @postinstall_kind = val
            when "postinstall_md5"
              @postinstall_md5 = val
            when "kernel"
              @kernel = val
            when "kernel_params"
              @kernel_params = val
            when "initrd"
              @initrd = val
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
      id = 0
      return check_md5_digest
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
                                             AND version=(SELECT MAX(version) FROM environments WHERE user=\"#{user}\")"
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
      @tarball_file = hash["tarball_file"]
      @tarball_kind = hash["tarball_kind"]
      @tarball_md5 = hash["tarball_md5"]
      @postinstall_file = hash["postinstall_file"]
      @postinstall_kind = hash["postinstall_kind"]
      @postinstall_md5 = hash["postinstall_md5"]
      @kernel = hash["kernel"]
      @kernel_params = hash["kernel_params"]
      @initrd = hash["initrd"]
      @part = hash["part"]
      @fdisk_type = hash["fdisk_type"]
      @filesystem = hash["filesystem"]
      @user = hash["user"]
      @environment_kind = hash["environment_kind"]
      @demolishing_env = hash["demolishing_env"]
    end

    # Check the MD5 digest of a file
    #
    # Arguments
    # * nothing
    # Output
    # * returns true if the digest is OK, false otherwise
    def check_md5_digest
      return ((Digest::MD5.hexdigest(File.read(@tarball_file)) == @tarball_md5) && (Digest::MD5.hexdigest(File.read(@postinstall_file)) == @postinstall_md5))
    end

    # Print the header
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def short_view_header
      puts "Name         Version     User            Description"
      puts "####         #######     ####            ###########"
    end

    # Print the short view
    #
    # Arguments
    # * nothing
    # Output
    # * nothing
    def short_view
      printf("%-15s %-7s %-10s %-40s\n", @name, @version, @user, @description)
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
      puts "tarball_file : #{@tarball_file}"
      puts "tarball_kind : #{@tarball_kind}"
      puts "tarball_md5 : #{@tarball_md5}"
      puts "postinstall_file : #{@postinstall_file}"
      puts "postinstall_kind : #{@postinstall_kind}"
      puts "postinstall_md5 : #{@postinstall_md5}"
      puts "kernel : #{@kernel}"
      puts "kernel_params : #{@kernel_params}"
      puts "initrd : #{@initrd}"
      puts "part : #{@part}"
      puts "fdisk_type : #{@fdisk_type}"
      puts "filesystem : #{@filesystem}"
      puts "user : #{@user}"
      puts "environment_kind : #{@environment_kind}"
      puts "demolishing_env : #{@demolishing_env}"
    end
  end
end
