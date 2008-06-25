require 'digest/md5'

module EnvironmentManagement
  class Environment
    attr_reader :tarball_file
    attr_reader :tarball_md5
    attr_reader :postinstall_file
    attr_reader :postinstall_md5
    attr_reader :kernel
    attr_reader :initrd
    attr_reader :kernel_params
    attr_reader :part
   
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
            when "initrd"
              @initrd = val
            when "kernel_params"
              @kernel_params = val
            when "part"
              @part = val
            else
              puts "#{attr} is an invalid attribute"
            end
          end
        }
      end
      check_md5_digest
    end

    def check_md5_digest
      if not ((Digest::MD5.hexdigest(File.read(@tarball_file)) == @tarball_md5) && (Digest::MD5.hexdigest(File.read(@postinstall_file)) == @postinstall_md5))
        raise "Invalid md5 checksum"
      end
    end

    def load_from_database

    end

    def to_s
      puts "Tarball: #{@tarball_file}, md5: #{@tarball_md5}"
      puts "Postinstall: #{@postinstall_file}, md5: #{@postinstall_md5}"
      puts "Kernel: #{@kernel}"
      puts "Initrd: #{@initrd}"
      puts "Partition: #{@part}"
    end
  end
end
