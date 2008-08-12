require 'digest/md5'
require 'lib/db'

module EnvironmentManagement
  class Environment
    attr_reader :name
    attr_reader :version
    attr_reader :description
    attr_reader :author
    attr_reader :tarball_file
    attr_reader :tarball_md5
    attr_reader :postinstall_file
    attr_reader :postinstall_md5
    attr_reader :kernel
    attr_reader :kernel_params
    attr_reader :initrd
    attr_reader :part
    attr_reader :fdisk_type
    attr_reader :filesystem
    attr_reader :user

    def load_from_file(file)
      if not File.exist?(file)
        raise "#{file} does not exist"
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
            when "tarball_md5"
              @tarball_md5 = val
            when "postinstall_file"
              @postinstall_file = val
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
            else
              puts "#{attr} is an invalid attribute"
            end
          end
        }
      end
      check_md5_digest
    end

    def load_from_database(name, version, user)
      
    end

    def load_from_hash(hash)
      @name = hash["name"]
      @version = hash["version"]
      @description = hash["description"]
      @author = hash["author"]
      @tarball_file = hash["tarball_file"]
      @tarball_md5 = hash["tarball_md5"]
      @postinstall_file = hash["postinstall_file"]
      @postinstall_md5 = hash["postinstall_md5"]
      @kernel = hash["kernel"]
      @kernel_params = hash["kernel_params"]
      @initrd = hash["initrd"]
      @part = hash["part"]
      @fdisk_type = hash["fdisk_type"]
      @filesystem = hash["filesystem"]
      @user = hash["user"]
    end

    def check_md5_digest
      if not ((Digest::MD5.hexdigest(File.read(@tarball_file)) == @tarball_md5) && (Digest::MD5.hexdigest(File.read(@postinstall_file)) == @postinstall_md5))
        raise "Invalid md5 checksum"
      end
    end

    def short_view_header
      puts "Name         Version     User            Description"
      puts "####         #######     ####            ###########"
    end

    def short_view
      printf("%-15s %-7s %-10s %-40s\n", @name, @version, @user, @description)
    end

    def full_view
      puts "name : #{@name}"
      puts "version : #{@version}"
      puts "description : #{@description}"
      puts "author : #{@author}"
      puts "tarball_file : #{@tarball_file}"
      puts "tarball_md5 : #{@tarball_md5}"
      puts "postinstall_file : #{@postinstall_file}"
      puts "postinstall_md5 : #{@postinstall_md5}"
      puts "kernel : #{@kernel}"
      puts "kernel_params : #{@kernel_params}"
      puts "initrd : #{@initrd}"
      puts "part : #{@part}"
      puts "fdisk_type : #{@fdisk_type}"
      puts "filesystem : #{@filesystem}"
      puts "user : #{@user}"
    end
  end
end
