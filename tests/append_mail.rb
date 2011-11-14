#!/usr/bin/env ruby

require 'net/imap'

imap = Net::IMAP.new "localhost", 10143
imap.login "rakoo", "test"


imap.append "~sagem", <<EOF.gsub(/\n/, "\r\n"), [:Seen, "~hotmail"]
From: coucou
To: test
Message-ID: <a@domain>


message du machin

EOF


