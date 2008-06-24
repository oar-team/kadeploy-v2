module PXEOperations
  class PXEOps
    def hexalize(n)
      return sprintf("%02X", n)
    end

    def hexalize_ip(ip)
      res = String.new
      ip.split(".").each { |v|
        res.concat(hexalize(v))
      }
      return res
    end
    
    def write_pxe(ips, msg, tftp_repository, tftp_cfg)
      ips.each { |ip|
        file = tftp_repository + "/" + tftp_cfg + "/" + hexalize_ip(ip)
        #prevent from overwriting some linked files
        if File.exist?(file) then
          File.delete(file)
        end
        f = File.new(file, File::CREAT|File::RDWR, 0644)
        f.write(msg)
      }
    end
    
    def set_pxe_for_linux(ips, kernel, initrd, boot_part, tftp_repository, tftp_img, tftp_cfg) 
      prompt = 1
      display = "messages"
      timeout = 50
      baudrate = 38400
      template = "PROMPT #{prompt}\nSERIAL 0 #{baudrate}\nDEFAULT bootlabel\nDISPLAY #{display}\nTIMEOUT #{timeout}\n\nlabel bootlabel\n";
      kernel_line = "\tKERNEL " + tftp_repository  + "/" + tftp_img + "/" + kernel + "\n"
      initrd_line = "\tINTIRD initrd=" + tftp_repository  + "/" + tftp_img + "/" + initrd + " root=" + boot_part + "\n"
      msg = template + kernel_line + initrd_line
      write_pxe(ips, msg, tftp_repository, tftp_cfg)
    end
  end
end


