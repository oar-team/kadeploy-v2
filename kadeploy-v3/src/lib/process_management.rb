#Kadeploy libs
require 'cmdctrl_wrapper'

module ProcessManagement
  private
  # Get the childs of a process
  #
  # Arguments
  # * father: pid of the father
  # Output
  # * return an Array of the pid
  def ProcessManagement::get_childs(father)
    result = Array.new
    cmd = "ps -eo pid,ppid |grep #{father}"
    pr = CmdCtrlWrapper::init
    CmdCtrlWrapper::add_cmd(pr, cmd, "none")
    CmdCtrlWrapper::run(pr)
    tab = CmdCtrlWrapper::get_output(pr).split("\n")
    tab.each { |line|
      line =~ /\A[\ ]*(\d+)[\ ]*(\d+)[\ ]*\Z/
      content = Regexp.last_match
      pid = content[1].to_i
      ppid = content[2].to_i
      if (ppid == father) then
        result.push(pid)
      end
    }
    return result
  end

  # Kill a process subtree
  #
  # Arguments
  # * father: pid of the father
  # Output
  # * nothing
  def ProcessManagement::kill_tree(father)
    finished = false
    while not finished
      list = ProcessManagement::get_childs(father)
      if not list.empty? then
        list.each { |pid|
          ProcessManagement::kill_tree(pid)
          Process.kill(9, pid)
        }
      else
        finished = true
      end
    end
  end

  public

  # Kill a process and all its childs
  #
  # Arguments
  # * father: pid of the process
  # Output
  # * nothing
  def ProcessManagement::killall(pid)
    ProcessManagement::kill_tree(pid)
    Process.kill(9, pid)
  end
end
