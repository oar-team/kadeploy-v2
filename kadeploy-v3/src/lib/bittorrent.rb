#Kadeploy libs
require 'cmdctrl_wrapper'

#Ruby libs
require 'tempfile'

module Bittorrent
  private

  def Bittorrent::get_remaining_download(torrent_hash, tracker_ip, tracker_port)
    #first, we get a temporary file
    temp = Tempfile.new("bttrack_wget")
    #then, we grab the HTML output of bttrack
    cmd = "wget --quiet -O #{temp} http://#{tracker_ip}:#{tracker_port} ;  grep {torrent_hash} #{temp} | sed 's/\"//g'"
    CmdCtrlWrapper::add_cmd(pr, cmd, "none")
    CmdCtrlWrapper::run(pr)
    html_output = CmdCtrlWrapper::get_output(pr)
    temp.unlink
    
    if /<tr><td.+\/td><td.+\/td><td align=right><code>(\d+)<\/code><\/td><td.+\/td><\/tr>/ =~ html_output then
      content = Regexp.last_match
      nb = content[1]
      return nb
    else
      return -1
    end
  end


  public

  # Make the torrent file
  #
  # Arguments
  # * torrent: name of the torrent
  # * filename: name of the file
  # * tracker_ip: ip of the bittorrent tracker
  # * tracker_port: port of the bittorrent tracker
  # Output
  # * return true if the torrent file has been correctly generated, false otherwise
  def Bittorrent::make_torrent(torrent, filename, tracker_ip, tracker_port)
    cmd = "btmakemetafile --target #{torrent} #{file} http://#{tracker_ip}:#{tracker_port}/announce"
    return system(cmd)
  end

  # Launch a local seed
  #
  # Arguments
  # * torrent: name of the torrent
  # Output
  # * return the pid of the forked process, -1 if the operation has not been performed correcty
  def Bittorrent::launch_seed(torrent)
    cmd = "btdownloadheadless #{torrent}"
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
  def Bittorrent::wait_end_of_download(timeout, torrent)
    finished = false
    start = Time.now.to_i
    while ((Time.now.to_i - start) < timeout) && (not finished)
      if (get_remaining_download(torrent) > 0) then
        finished = true
      else
        sleep(10)
      end
    end
    return finished
  end
end
