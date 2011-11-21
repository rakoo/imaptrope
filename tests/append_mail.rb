#!/usr/bin/env ruby

require 'net/imap'

imap = Net::IMAP.new "localhost", 10143
imap.login "rakoo", "test"


imap.append "~test", <<EOF.gsub(/\n/, "\r\n"), [:Seen, "~hotmail"]
From: coucou@domain
to: test@domain


message du machin

EOF


