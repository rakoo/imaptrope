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
require 'leveldb'

class IMAPTrope

  MailboxStatus = Struct.new(:messages, :recent, :uidnext, :uidvalidity, :unseen)

	class HeliotropeFakeMailbox		

		attr_reader :mail_store, :name 

		def initialize(mail_store, name, data)
			@mail_store = mail_store
			@name = name # the label: the mailbox should be accessed with ~label
			@data = data

			@heliotropeclient = @data['heliotrope-client']
		end

		def query
			NullQuery.new
		end

		def append_mail_to_mailbox(message, flags)

			# separate between state flags and heliotrope labels
			state = (flags & MailStore::MESSAGE_STATE.to_a).compact
			state.map!{ |f| format_label_from_imap_to_heliotrope!(f) }.compact.uniq!

			labels = (flags - MailStore::MESSAGE_STATE.to_a).to_a.compact
			labels.map!{ |f| format_label_from_imap_to_heliotrope!(f) }.compact.uniq!
			hlabel = format_label_from_imap_to_heliotrope!(@name)
			labels = (labels + [hlabel]).compact.uniq

			#validate the message
			# TODO: use a timeout
			validated_message = begin
				Message.validate(message)
			rescue InvalidMessageError => e
				puts "; [ERROR] Invalid message : #{e.inspect}"
				# pass the message as-is, pray it works
				message
			end

			validated_message = validated_message.force_encoding("binary") if
				validated_message.respond_to?(:force_encoding)

			# try to add the message
			status = @heliotropeclient.add_message(validated_message, :labels => labels, :state => state)

			if status["status"] == "seen"
				# Message already exists; set labels and state
				message_id = status["doc_id"]
				puts "; adding labels #{labels} and state #{state} to message_id #{message_id} in hmailbox #{hlabel} "

				# message may hove been deleted, so it doesn't exist anymore in
				# the uids association
				old_flags = @mail_store.fetch_labels_and_flags_for_message_id message_id
				new_flags = (old_flags + flags + [@name]).flatten.compact.uniq

				status = @mail_store.set_labels_and_flags_for_message_id(message_id, new_flags)
			else
				puts "; added message to hmailbox #{hlabel} with labels #{labels} and state #{state}"
			end

			# write the hash to relate UIDs in this mailbox to message_id in
			# heliotrope
			uid_to_store = next_uid
			message_id = status["doc_id"] || status["message_id"]
			write_uids uids.merge({uid_to_store => message_id})
			increment_next_uid

			return {
				:status => :unseen,
				:uid => uid_to_store
			}	
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
			mailbox_status.uidnext = next_uid

			if @name == "All Mail"
				mailbox_status.messages = @heliotropeclient.size
				mailbox_status.unseen = @heliotropeclient.count "~unread"
			elsif MailStore::SPECIAL_MAILBOXES.member?(@name)
				mailbox_status.messages = @heliotropeclient.count "~#{MailStore::SPECIAL_MAILBOXES[@name]}"
				mailbox_status.unseen = @heliotropeclient.count "~unread+~#{MailStore::SPECIAL_MAILBOXES[@name]}"
			else
				mailbox_status.messages = @heliotropeclient.count "#{@name}"
				mailbox_status.unseen = @heliotropeclient.count "~unread+#{@name}"
			end

			mailbox_status
		end

		def uid_search(query)
			new_query = format(query)

			result = @mail_store.search_in_heliotrope new_query  # fetches threads

			thread_ids = []
			result.each do |thread|
				# iterates through threads to get ids
				thread_ids << thread.fetch("thread_id").to_i
			end

			message_ids = []
			thread_ids.each do |id|
				thread = @heliotropeclient.thread id # get thread messages
				thread.each do |messageinfos|
					message_ids <<  messageinfos.first["message_id"] # get message id
				end
			end

			message_ids.map{|m| uid_for_message_id m}
		end

		def fetch(sequence_set)
			puts "; as sequence_set"
			# mails_in_mailbox contains all the mails in the mailbox. When the
			# client want the message 1..3 (sequence_set), he wants the 3 first
			# messages in the mailbox; imaptrope gives him message_ids_as_seq.at(1),
			# message_ids_as_seq.at(2) and message_ids_as_seq.at(3)
			fetch_internal(sequence_set, message_ids_as_seq)
		end

		def uid_fetch(sequence_set)
			puts "; as uid"
			# Same thing for uids
			fetch_internal(sequence_set, message_ids_as_uids)
		end

		def seqno_for_message_id(message_id)
			message_ids_as_seq.key(message_id)
		end

		def uid_for_message_id(message_id)
			uid = message_ids_as_uids.key(message_id)
		end

		def remove_mail(uid_to_remove)
			# remove mail from uids list, after removing its flags (with
			# delete_seqno, just after
			uids_list = message_ids_as_uids
			uids_list.delete uid_to_remove
			write_uids uids_list
		end

		def delete_seqno(seqno)
			message_id_to_delete = message_ids_as_seq[seqno]
			uid_to_delete = uid_for_message_id(message_id_to_delete)
			

			flags = @mail_store.fetch_labels_and_flags_for_message_id message_id_to_delete
			puts "; deleting message #{message_id_to_delete}"
			begin
				raise NotToDeleteError.new("Trying to realdelte message_id #{message_id_to_delete} not marked to delete") unless 
					flags.include?("\\Deleted")
			rescue Exception => e
					puts e.inspect
			end

			flags -= [@name]
			flags -= ["\\Deleted"]
			flags -= ["~deleted"]
			@mail_store.set_labels_and_flags_for_message_id message_id_to_delete, flags


			# verify that flags were removed
			flags_verify = @mail_store.fetch_labels_and_flags_for_message_id message_id_to_delete
			puts "; flags_verify : #{flags_verify} should not contain #{@name}"
			return 0 if flags_verify.include?(@name)
			remove_mail uid_to_delete
			seqno
		end

		private

		def message_ids_as_uids
			if @name == "All Mail"
				# there is no uids_list because all mails are added to All Mail
				# anyway, so it's { 1 => 1, 2 => 2, ...}
				out = {}
				(1..@heliotropeclient.size).each.with_index { |value, index| out[index+1] = value}
				return out
			else
				return uids
			end
		end

		def message_ids_as_seq
			# get the sequence numbers of the messages in the mailbox

			query = format(@name)
			threads_in_mailbox = if query.nil? or query.empty?
				(1..@heliotropeclient.size).to_a
			else
				@mail_store.search_in_heliotrope(query).map{|threadinfos| threadinfos["thread_id"]}
			end.sort!

			mails_in_mailbox = threads_in_mailbox.map do |thread_id| 
				@heliotropeclient.thread(thread_id).map{ |blob| blob.first }.map{|message| message["message_id"]}
			end.flatten.sort.unshift("shift")

			out = {}

			mails_in_mailbox.each.with_index do |message_id, index|
				out[index] = message_id
			end

			out
		end

		def format(query)
			queryterms = query.to_s.split("+").map do |term|
				if MailStore::SPECIAL_MAILBOXES.include?(term)
					"~" + MailStore::SPECIAL_MAILBOXES.fetch(term) unless MailStore::SPECIAL_MAILBOXES.fetch(term).nil?
				else
					term
				end
			end.join("+")

			if @name != "All Mail" && !queryterms.include?(@name)
					queryterms << "+" << "#{@name}" 
			end

			CGI.unescape(queryterms)
		end

		def fetch_internal(sequence_set, assoc)

			puts "; fetching #{sequence_set} in #{assoc}"
			
			mails = []
# assoc contains all the mails in the mailbox. When the
# client want the message 1..3 (sequence_set), he wants the 3 first
# messages in the mailbox; imaptrope gives him assoc.fetch(1),
# assoc.fetch(2) and assoc.fetch(3)
	
			sequence_set.each do |atom|
				case atom
				when Range
					if atom.last == -1
						atom.last = assoc.keys.last
					end
					atom.each do |ind|
						messageinfos = @heliotropeclient.messageinfos assoc.fetch(ind)
						mails.push(Message.new(messageinfos, self, @heliotropeclient))
					end
				else
					messageinfos = @heliotropeclient.messageinfos assoc.fetch(atom)
					mails.push(Message.new(messageinfos, self, @heliotropeclient))
				end
			end

			mails
		end

		# correspondance between uid in this mailbox
		# and message_id in heliotrope (which is absolute)

		def next_uid
			key = "#{@name}/next"
			@mail_store.next_uid key
		end

		def increment_next_uid
			key = "#{@name}/next"
			value = @mail_store.next_uid(key)
			@mail_store.increment_next_uid key, value + 1
		end

		def uids
			key = "#{@name}/UIDs"
	 		@mail_store.get_uids key
		end

		def write_uids value
			key = "#{@name}/UIDs"
			@mail_store.write_uids key, value
		end

		def format_label_to_imap!(label)
			unless /^\~/.match(label) or MailStore::SPECIAL_MAILBOXES.key?(label)
				raise NoMailboxError.new("#{label} doesn't exist or is invalid")
			end
		end


		def format_label_from_imap_to_heliotrope!(ilabel)
			if MailStore::SPECIAL_MAILBOXES.key?(ilabel)
				return MailStore::SPECIAL_MAILBOXES.fetch(ilabel)
			elsif /^\\/.match(ilabel)
				raise NoMailboxError.new("#{ilabel} is not a valid name!")
			else
				return ilabel.gsub(/^\~/,"") # remove ~ from the beginning of the label
			end
		end

	end
end
