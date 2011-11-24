# $Id$
# Copyright (C) 2005  Shugo Maeda <shugo@ruby-lang.org>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

require 'cgi'

class Ximapd

  MailboxStatus = Struct.new(:messages, :recent, :uidnext, :uidvalidity, :unseen)

	class HeliotropeFakeMailbox		

		attr_reader :mail_store, :name 

		def initialize(mail_store, name, data)
			@mail_store = mail_store
			@name = name # the label: the mailbox should be accessed with ~label ,  not just with label
			@data = data

			@heliotropeclient = @data['heliotrope-client']
		end

		def query
			NullQuery.new
		end


		def status
			# http://www.faqs.org/rfcs/rfc3501.html : mailbox status has these
			# fields : 
			# - MESSAGES : counts the number of messages in this mailbox
			# - RECENT : number of messages with the \Recent FLAG
			# - UIDNEXT : uid that will be assigned to the next mail to be stored
			# - UIDVALIDITY : int. If it has changed between 2 sessions, it means
			# the mailbox isn't valid, and the client needs to redownload messages
			# from the beginning
			# - UNSEEN : number of messages without the \Seen FLAG

			mailbox_status = MailboxStatus.new
			mailbox_status.recent = @heliotropeclient.count "\\Recent" #this label/FLAG doesn't exist, but I don't know what we can do with it anyway
			#cheat : uid should be unique only to a mailbox, but 2 messages can have the same uid if they don't belong to the same mailbox;
			# here we already know what will be the next uid, even if it is not linked to the mailbox
			mailbox_status.uidnext = @heliotropeclient.size + 1 


			mailbox_status
		end

		def uid_search(query)
			new_query = format(query)

			result = @heliotropeclient.search new_query  # fetches threads

			thread_ids = []
			result.each do |thread|
				# iterates through threads to get ids
				thread_ids << thread.fetch("thread_id").to_i
			end

			uids = []
			thread_ids.each do |id|
				thread = @heliotropeclient.thread id # get thread messages
				thread.each do |messageinfos|
					uids <<  messageinfos.first["message_id"] # get message id
				end
			end

			uids
		end

		def fetch(sequence_set)
			mails = []

			query = format(@name)
			if query.nil? or query.empty?
				threads_in_mailbox = (1..@heliotropeclient.size).to_a
			else
				threads_in_mailbox = @heliotropeclient.search(query).map{|threadinfos| threadinfos["thread_id"]}
			end

			threads_in_mailbox.sort!
			puts "; threads in mailbox : #{threads_in_mailbox}"

			mails_in_mailbox = threads_in_mailbox.map do |thread_id| 
				messageinfos = @heliotropeclient.thread(thread_id).map { |blob| blob.first }
				messageinfos.first["message_id"]
			end.flatten.sort
			puts "; mails in mailbox : #{mails_in_mailbox}"

# mails_in_mailbox contains all the mails in the mailbox. When the
# client want the message 1..3 (sequence_set), he wants the 3 first
# messages in the mailbox; imaptrope gives him mails_in_mailbox.at(1),
# mails_in_mailbox.at(2) and mails_in_mailbox.at(3)
	
			sequence_set.each do |atom|
				case atom
				when Range
					atom.each do |uid|
						messageinfos = @heliotropeclient.message mails_in_mailbox.at(uid - 1)
						mails.push(Message.new(messageinfos, @mail_store))
					end
				else
					messageinfos = @heliotropeclient.message mails_in_mailbox.at(atom - 1)
					mails.push(Message.new(messageinfos, @mail_store))
				end
			end
			mails
		end

		def uid_fetch(sequence_set)
			mails = []
			sequence_set.each do |atom|
				case atom
				when Range
					atom.each do |uid|
						messageinfos = @heliotropeclient.message uid
						mails.push(Message.new(messageinfos, @mail_store))
					end
				else
					messageinfos = @heliotropeclient.message atom
					mails.push(Message.new(messageinfos, @mail_store))
				end
			end
			mails
		end



		private


		def format(query)
			queryterms = query.to_s.split("+")
			out = queryterms.map! do |term|
				if MailStore::SPECIAL_MAILBOXES.include?(term)
					"~" + MailStore::SPECIAL_MAILBOXES.fetch(term) unless MailStore::SPECIAL_MAILBOXES.fetch(term).nil?
				else
					term
				end
			end.join("+")

			if @name != "All Mail" && !queryterms.include?(@name)
					out << "+" << "#{@name}" 
			end

			CGI.unescape(out)
		end

	end
end
