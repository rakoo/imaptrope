#!/usr/bin/ruby

require 'net/imap'
require 'mail'

BASE_PATH = "/home/rakoo/.offlineimap/Repository-imaptrope/UIDMapping/"

imap = Net::IMAP.new "imap.gmail.com", 993, true
imap.login "salutlesamis.coucou", "coucoupassword"

imaptrope = Net::IMAP.new "localhost", 10143
imaptrope.login "rakoo", "test"

Dir.foreach(BASE_PATH) do |file|
	next unless /^\~/.match file
	next unless /~(test|coucou)/.match file

	puts "scanning #{file}..."
	imaptrope.select file
	imap.select file.gsub(/^\~/,"")

	File.open(BASE_PATH + "/" + file, "r") do |infile|
		while (line = infile.gets)
			imaptrope_uid = line.split(/:/).first.to_i
			imap_uid = line.split(/:/).last.to_i


			raw_gmail = imap.uid_fetch(imap_uid, "BODY[]").first.attr["BODY[]"]
			mail_gmail = Mail.read_from_string raw_gmail
			subject_gmail = mail_gmail.subject

			raw_imaptrope = imaptrope.uid_fetch(imaptrope_uid, "BODY[]").first.attr["BODY[]"]
			mail_imaptrope = Mail.read_from_string raw_imaptrope
			subject_imaptrope = mail_imaptrope.subject

			if subject_gmail != subject_imaptrope 
				puts "subject differ !"
			end
			puts "	gmail[#{imap_uid}] : #{subject_gmail}"
			puts "	imaptrope[#{imaptrope_uid}] : #{subject_imaptrope}"
		end
	end
end

