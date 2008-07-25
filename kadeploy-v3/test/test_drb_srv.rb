#!/usr/bin/ruby -w

require 'drb'
require 'thread'

class TestServer
  def doit(titi)
    Thread.new {
      puts "Hello, Distributed World"
      puts titi
    }
  end
end


aServerObject = TestServer.new
DRb.start_service('druby://g5kdev:19000', aServerObject)
DRb.thread.join # Don't exit just yet!
