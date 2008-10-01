require 'ftools'

module Cache
  private

  # Get the size of a directory (including sub-dirs)
  # Arguments
  # * dir: dirname
  # Output
  # * returns the size in bytes if the directory exist, 0 otherwise
  def Cache::get_dir_size(dir)
    sum = 0
    if FileTest.directory?(dir) then
      Dir.foreach(dir) { |f|
        if (f != ".") && (f != "..") then
          if FileTest.directory?(dir + "/" + f) then
            sum += get_dir_size(dir + "/" + f)
          else
            sum += File.stat(dir + "/" + f).size
          end
        end
      }
    end
    return sum
  end

  public

  # Clean a cache according an LRU policy
  #
  # Arguments
  # * dir: cache directory
  # * max_size: maximum size for the cache in Bytes
  # * time_before_delete: time in hours before a file can be deleted
  # * pattern: pattern of the files that might be deleted
  # Output
  # * nothing
  def Cache::clean_cache(dir, max_size, time_before_delete, pattern)
    no_change = false
    while (get_dir_size(dir) > max_size) && (not no_change)
      lru = ""
      Dir.foreach(dir) { |f|
        if ((f =~ pattern) == 0) && (f != "..") && (f != ".") then
          access_time = File.atime(dir + "/" + f).to_i
          now = Time.now.to_i
          #We only delete the file older than a given number of hours
          if  ((now - access_time) > (60 * 60 * time_before_delete)) && ((lru == "") || (File.atime(lru).to_i > access_time)) then
            lru = dir + "/" + f
          end
        end
      }
      if (lru != "") then
        File.delete(lru)
      else
        no_change = true
      end
    end
  end
end
