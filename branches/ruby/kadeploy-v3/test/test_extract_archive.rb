#!/usr/bin/ruby -w
require '../lib/microsteps'

ms = MicroStepsLibrary::MicroSteps.new(nil, nil, nil, nil, nil)

archive = "/home/ejeanvoi/etch.tgz"
file_array = Array.new
file_array.push("boot/vmlinuz-2.6.18-5-686")
file_array.push("boot/initrd.img-2.6.18-5-686")
dest_dir = "/tmp/titi"

ms.extract_files_from_archive(archive, file_array, dest_dir)
