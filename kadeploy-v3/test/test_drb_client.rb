#!/usr/bin/ruby -w

require 'drb'


DRb.start_service()
obj = DRbObject.new(nil, 'druby://g5kdev:19000')
# Now use obj
1.upto(1000) { |n|
  obj.doit(n)
}
