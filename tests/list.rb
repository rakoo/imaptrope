#!/usr/bin/env ruby

require 'net/imap'

imap = Net::IMAP.new "localhost", 10143
imap.login "rakoo", "test"


imap.list("", "*").each do |m|
	puts m.inspect
end
