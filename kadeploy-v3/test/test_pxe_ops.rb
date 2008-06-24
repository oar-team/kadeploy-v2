#!/usr/bin/ruby -w

require "../lib/pxe_ops.rb"


test = PXEOperations::PXEOps.new
puts test.hexalize(192)
puts test.hexalize_ip("192.168.0.10")

ips = Array.new
ips.push("192.168.0.1")
ips.push("192.168.0.2")
ips.push("192.168.0.3")
ips.push("192.168.0.4")
ips.push("192.168.0.5")
ips.push("192.168.0.6")

if not File.exist?("test-dir") then
  Dir.mkdir("test-dir")
end

test.set_pxe_for_linux(ips, "linux-image-2.6.99", "linux-initrd-2.6.99.img ETH_DRV=e1000 ETH_DEV=eth0 DISK_DRV=ahci console=tty0 console=ttyS1,38400n8 ramdisk_size=400000", "/dev/sda2", "pxe", "img", "pxelinux.cfg")

