#Kadeploy libs
require 'cmdctrl_wrapper'

#Ruby libs
require 'tempfile'
require 'process_management'

module Bittorrent
  DEFAULT_BITTORRENT_PORT = 6969

  private
  # Get the hash reference of a torrent
  #
  # Arguments
  # * torrent: path to the torrent file
  # Output
  # * return the hash reference of the torrent
  def Bittorrent::get_torrent_hash(torrent)
    cmd = "btshowmetainfo #{torrent} | grep hash | sed 's/info hash.....: //'"
    pr = CmdCtrlWrapper::init
    CmdCtrlWrapper::add_cmd(pr, cmd, "none")
    CmdCtrlWrapper::run(pr)
    hash = CmdCtrlWrapper::get_output(pr)
    return hash.chomp
  end

  # Get the remaining leechers of a torrent
  #
  # Arguments
  # * torrent_hash: hash reference to a torrent
  # * track_ip: ip of the tracker
  # * tracker_port: port of the tracker
  # Output
  # * return the number of remaining leechers or -1 if something went wrong
  def Bittorrent::get_remaining_download(torrent_hash, tracker_ip, tracker_port)
    #first, we get a temporary file
    temp = Tempfile.new("bttrack_wget")
    #then, we grab the HTML output of bttrack
    cmd = "wget --quiet -O #{temp.path} http://#{tracker_ip}:#{tracker_port} ; grep #{torrent_hash} #{temp.path} | sed 's/\"//g'"
    pr = CmdCtrlWrapper::init
    CmdCtrlWrapper::add_cmd(pr, cmd, "none")
    CmdCtrlWrapper::run(pr)
    html_output = CmdCtrlWrapper::get_output(pr)
    temp.unlink
    if /<tr><td.+\/td><td.+\/td><td align=right><code>(\d+)<\/code><\/td><td.+\/td><\/tr>/ =~ html_output then
      content = Regexp.last_match
      nb = content[1].to_i
      return nb
    else
      return -1
    end
  end


  public

  # Launch a Bittorrent tracker
  #
  # Arguments
  # * file: filename of the download state file
  # Output
  # * return the pid of the tracker and its port
  def Bittorrent::launch_tracker(file)
    port = DEFAULT_BITTORRENT_PORT
    try_another_port = true
    while try_another_port
      pid = Process.fork {
        exec("bttrack --port #{port} --dfile file &>/dev/null" )
      }
      sleep(2)
      if Process.waitpid(pid, Process::WNOHANG) then
        port += 1
      else
        try_another_port = false
      end
    end
    return pid, port
  end

  # Make the torrent file
  #
  # Arguments
  # * filename: name of the file
  # * tracker_ip: ip of the bittorrent tracker
  # * tracker_port: port of the bittorrent tracker
  # Output
  # * return true if the torrent file has been correctly generated, false otherwise
  def Bittorrent::make_torrent(filename, tracker_ip, tracker_port)
    cmd = "btmakemetafile #{filename} http://#{tracker_ip}:#{tracker_port}/announce &>/dev/null"
    return system(cmd)
  end

  # Launch a local seed
  #
  # Arguments
  # * torrent: name of the torrent
  # * kadeploy_cache: path to the kadeploy cache
  # Output
  # * return the pid of the forked process, -1 if the operation has not been performed correcty
  def Bittorrent::launch_seed(torrent, kadeploy_cache)
    cmd = "cd #{kadeploy_cache} ; btdownloadheadless #{torrent} &>/dev/null"
    pid = fork {
      exec(cmd)
    }
    if (pid == nil) then
      return -1
    else
      return pid
    end
  end

  # Get the hash of a torrent
  #
  # Arguments
  # * torrent: name of the torrent
  # Output
  # * return the hash of the torrent
  def Bittorrent::get_file_hash(torrent)
    cmd = "btshowmetainfo #{torrent} |grep hash|sed 's/.*:\ //g'"
    pr = CmdCtrlWrapper::init
    CmdCtrlWrapper::add_cmd(pr, cmd, "none")
    CmdCtrlWrapper::run(pr)
    return CmdCtrlWrapper::get_output(pr)
  end  

  # Wait the end of the download
  #
  # Arguments
  # * timeout: timeout
  # * torrent: name of the torrent
  # Output
  # * return true if the download is finished before the timeout, false otherwise
  def Bittorrent::wait_end_of_download(timeout, torrent, tracker_ip, tracker_port)
    finished = false
    start = Time.now.to_i
    torrent_hash = get_torrent_hash(torrent)
    while ((Time.now.to_i - start) < timeout) && (not finished)
      if (get_remaining_download(torrent_hash, tracker_ip, tracker_port) == 0) then
        finished = true
      else
        sleep(2)
      end
    end
    return finished
  end
end
