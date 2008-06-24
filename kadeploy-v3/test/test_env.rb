#!/usr/bin/ruby -w

require "../lib/environment"


env = EnvironmentManagement::Environment.new
env.load_from_file("envfile.dsc")
env.to_s
