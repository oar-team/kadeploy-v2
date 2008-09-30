module PXEOperations
  private
  # Compute the hexalized value of a decimal number
  #
  # Arguments
  # * n: decimal number to hexalize
  # Output
  # * hexalized value of n
  def PXEOperations::hexalize(n)
    return sprintf("%02X", n)
  end

  # Compute the hexalized representation of an IP
  #
  # Arguments
  # * ip: string that contains the ip to hexalize
  # Output
  # * hexalized value of ip
  def PXEOperations::hexalize_ip(ip)
    res = String.new
    ip.split(".").each { |v|
      res.concat(hexalize(v))
    }
    return res
  end
  
  # Write the PXE information related to the group of nodes involved in the deployment
  #
  # Arguments
  # * ips: array of ip (aaa.bbb.ccc.ddd string representation)
  # * msg: string that must be written in the PXE configuration
  # * tftp_repository: absolute path to the TFTP repository
  # * tftp_cfg: relative path to the TFTP configuration repository
  # Output
  # * returns true in case of success, false otherwise
  # Fixme
  # * should do something if the PXE configuration cannot be written
  def PXEOperations::write_pxe(ips, msg, tftp_repository, tftp_cfg)
    ips.each { |ip|
      file = tftp_repository + "/" + tftp_cfg + "/" + hexalize_ip(ip)
      #prevent from overwriting some linked files
      if File.exist?(file) then
        File.delete(file)
      end
      f = File.new(file, File::CREAT|File::RDWR, 0644)
      f.write(msg)
      f.close
    }
    return true
  end
  
  public
  # Modify the PXE configuration
  #
  # Arguments
  # * ips: array of ip (aaa.bbb.ccc.ddd string representation)
  # * kernel: basename of the vmlinuz file
  # * initrd: basename of the initrd file
  # * boot_part: path of the boot partition
  # * tftp_repository: absolute path to the TFTP repository
  # * tftp_img: relative path to the TFTP image repository
  # * tftp_cfg: relative path to the TFTP configuration repository
  # Output
  # * returns the value of write_pxe
  def PXEOperations::set_pxe_for_linux(ips, kernel, initrd, boot_part, tftp_repository, tftp_img, tftp_cfg) 
    prompt = 1
    display = "messages"
    timeout = 50
    baudrate = 38400
    template = "PROMPT #{prompt}\nSERIAL 0 #{baudrate}\nDEFAULT bootlabel\nDISPLAY #{display}\nTIMEOUT #{timeout}\n\nlabel bootlabel\n";
    kernel_line = "\tKERNEL " + tftp_img + "/" + kernel + "\n"
    initrd_line = "\tAPPEND initrd=" + tftp_img + "/" + initrd 
    initrd_line += " root=" + boot_part if (boot_part != "")
    initrd_line += "\n"
    msg = template + kernel_line + initrd_line
    return write_pxe(ips, msg, tftp_repository, tftp_cfg)
  end
end


