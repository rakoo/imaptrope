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

require "heliotrope-client"
#require "heliotrope-backend"
require "set"
require "rest_client"
require "json"
require "leveldb"

class Ximapd


  module DataFormat
    module_function

    def quoted(s)
      if s.nil?
        return "NIL"
      else
        return format('"%s"', s.to_s.gsub(/[\r\n]/, "").gsub(/[\\"]/n, "\\\\\\&"))
      end
    end

    def literal(s)
      return format("{%d}\r\n%s", s.length, s)
    end
  end

  class MailData
    attr_reader :raw_data, :uid, :flags, :internal_date, :text, :properties
    attr_reader :parsed_mail

    def initialize(raw_data, uid, flags, internal_date, text, properties,
                   parsed_mail)
      @raw_data = raw_data
      @uid = uid
      @flags = flags
      @internal_date = internal_date
      @text = text
      @properties = properties
      @parsed_mail = parsed_mail
    end

    def to_s
      return @raw_data
    end

    def header
      return @parsed_mail.header
    end

    def multipart?
      return @parsed_mail.multipart?
    end

    def body
      return @parsed_mail.body
    end
  end

  class NullMessage
    attr_reader :header, :body

    def initialize
      @header = {}
      @body = ""
    end

    def multipart?
      return false
    end
  end

  class MailStore
    include MonitorMixin

		MESSAGE_IMMUTABLE_STATE = Set.new %w(attachment signed encrypted draft)

		MESSAGE_MUTABLE_STATE_HASH = {
			"\\Starred"	=>	"starred",
			"\\Seen"	=>	nil,
			"\\Deleted"	=>	"deleted",
			nil	=>	"unread"
		}
	
		MESSAGE_STATE = Set.new(MESSAGE_MUTABLE_STATE_HASH.values) + MESSAGE_IMMUTABLE_STATE

		SPECIAL_MAILBOXES = MESSAGE_MUTABLE_STATE_HASH.merge( 
		{
			"\\Answered" => nil,
		 	"\\Draft" => "draft",
			"Sent"	=> "sent",
		 	"All Mail" => nil,
		 	"INBOX" => "inbox"
		})
		# misses \Answered \Seen (the latter is problematic)
		# this Hash relates IMAP keywords with their Heliotrope
		# counterparts

		attr_reader :heliotropeclient
    attr_reader :config, :path, :mailbox_db, :mailbox_db_path
    attr_reader :plugins
    attr_reader :uid_seq, :uidvalidity_seq, :mailbox_id_seq
		attr_accessor :current_mailbox, :uid_store

    def initialize(config)
      super()
      @config = config
      @logger = @config["logger"]
			@heliotropeclient = HeliotropeClient.new "http://localhost:8043"

      @path = File.expand_path(@config["data_dir"])
      FileUtils.mkdir_p(@path)
      uidvalidity_seq_path = File.expand_path("uidvalidity.seq", @path)
      @uidvalidity_seq = Sequence.new(uidvalidity_seq_path)

      lock_path = File.expand_path("lock", @path)
      @lock = File.open(lock_path, "w+")
      @lock_count = 0

			if @uidvalidity_seq.current.nil?
				@uidvalidity_seq.current = 1
			end

			# this is used to fake the imap client exists so that it can
			# select it
			@fakemailboxes = []

			@current_mailbox = ""

			# UIDs ore unique to a mailbox, but this is incompatible with
			# heliotrope, where message_ids are unique through the whole mailstore
			@uid_store = LevelDB::DB.new File.join(@path, "UID_store")
    end

    def close
      @lock.close
    end

    def teardown
#@backend.teardown
    end

    def lock
      mon_enter
      if @lock_count == 0
        @lock.flock(File::LOCK_EX)
        #@backend.standby
      end
      @lock_count += 1
    end

    def unlock
      @lock_count -= 1
      if @lock_count == 0 && !@lock.closed?
        #@backend.relax
        @lock.flock(File::LOCK_UN)
      end
      mon_exit
    end

    def synchronize
      lock
      begin
        yield
      ensure
        unlock
      end
    end

    def write_last_peeked_uids
# don't need this
      #return if @last_peeked_uids.empty?
      #@mailbox_db.transaction do
        #@last_peeked_uids.each do |name, uid|
          #mailbox = @mailbox_db["mailboxes"][name]
          #if mailbox && mailbox["last_peeked_uid"] < uid
            #mailbox["last_peeked_uid"] = uid
          #end
        #end
        #@last_peeked_uids.clear
      #end
    end

    def mailboxes
			labels = Set.new(@heliotropeclient.labels)

			#Format to return: 
			#[
			# ["label1", "FLAGS for label1"],
			# ["label2", "FLAGS for label2"],
			#]

			out = []
			labels.each do |label|
				out << ["\~"+label, ""]	unless SPECIAL_MAILBOXES.value?(label)
			end

			SPECIAL_MAILBOXES.keys.each do |m|
				out << [m, ""]
			end

			@fakemailboxes.each do |m|
				out << m
			end

			out.uniq
    end

    def create_mailbox(name, query = nil)
			all_mailboxes = mailboxes
			format_label_to_imap!(name)
			unless all_mailboxes.assoc(name).nil? 
				raise MailboxExistError.new("#{name} already exists")
			end
			@fakemailboxes << [name, ""]
    end




    def delete_mailbox(name)
			format_label_to_imap!(name)
			raise MailboxError.new("Can't delete a special mailbox") if (SPECIAL_MAILBOXES.key?(name) or MESSAGE_IMMUTABLE_STATE.include?(name))

			all_mailboxes = mailboxes
			if all_mailboxes.assoc(name).nil? 
				raise MailboxExistError.new("#{name} doesn't exist")
			end

			#TODO : cannot delete if there is the \Noselect label or if there are
			#sublabels
			hlabel = format_label_from_imap_to_heliotrope!(name)
			
			count = @heliotropeclient.count name
			messageinfos = @heliotropeclient.search name, count # careful ! labels here are without the ~ !

			messageinfos.each do |m|
				new_labels = m["labels"]
			  new_labels -= [hlabel] 
				puts "old labels : #{m["labels"]}"
				puts "new labels : #{new_labels} "
				@heliotropeclient.set_labels!  m["thread_id"], new_labels
			end

			# prune when all is done
			@heliotropeclient.prune_labels!
    end




    def rename_mailbox(name, new_name)
			# TODO if a label has sublabels, there is a special treatment

		 	if (SPECIAL_MAILBOXES.key?(name) or SPECIAL_MAILBOXES.key?(new_name) or MESSAGE_IMMUTABLE_STATE.subset?(Set.new [name, new_name]))
				raise MailboxError.new("Can't rename a special mailbox")
			end

			all_mailboxes = mailboxes
			if all_mailboxes.assoc(name).nil? # mailbox "name" doesn't exist
				raise NoMailboxError.new("Can't rename #{name} to #{new_name} : #{name} doesn't exist")
			end
			unless all_mailboxes.assoc(new_name).nil? # mailbox "new_name" already exists
				raise MailboxExistError.new("Can't rename #{name} to #{new_name} : #{new_name} already exists")
			end

			puts "rename label #{name} to #{new_name}"

			hname = format_label_from_imap_to_heliotrope!(name)
			hnew_name = format_label_from_imap_to_heliotrope!(new_name)

			thread_infos = []
			count = @heliotropeclient.count name
			messageinfos = @heliotropeclient.search name, count # careful ! labels here are without the ~ !
			messageinfos.each do |m|
				new_labels = m["labels"]
				new_labels += [hnew_name]
			  new_labels -= [hname] unless hname == "inbox" # if the old label is inbox, duplicate instead of moving : RFC
				@heliotropeclient.set_labels!  m["thread_id"], new_labels
			end

			# prune labels
			@heliotropeclient.prune_labels!
    end

    def get_mailbox_status(mailbox_name, read_only = false)

			format_label_to_imap!(mailbox_name)
			mailbox = get_mailbox mailbox_name
			mailbox_status = mailbox.status
			mailbox_status.uidvalidity = @uidvalidity_seq.current
			return mailbox_status
    end

    #def index_mail(mail, filename)
      #begin
        #@backend.register(mail, filename)
        #s = mail.properties["x-ml-name"]
        #if !s.empty? && mail.properties["mailbox-id"] == 0 &&
          #!@mailbox_db["mailing_lists"].key?(s)
          #mbox_name = get_mailbox_name_from_x_ml_name(s)
          #mailbox_name = format("ml/%s", Net::IMAP.encode_utf7(mbox_name))
          #x_ml_name = mail.properties["x-ml-name"]
          #query = PropertyEqQuery.new("x-ml-name", x_ml_name).to_s
          #begin
            #create_mailbox_internal(mailbox_name, query)
            #mailbox = get_mailbox(mailbox_name)
            #mailbox["list_id"] = s
            #@mailbox_db["mailing_lists"][s] = {
              #"creator_uid" => mail.uid,
              #"mailbox" => mailbox_name
            #}
          #rescue MailboxExistError
          #end
        #end
      #rescue Exception => e
        #@logger.log_exception(e, "backend_mail")
      #end
    #end

    def get_mailbox(name)
			format_label_to_imap!(name)
			all_mailboxes = mailboxes
			mailbox_info = all_mailboxes.assoc(name)
			if mailbox_info.nil? # mailbox doesn't exist
				raise MailboxExistError.new("Can't select #{name} : this mailbox doesn't exist")
			elsif /\\Noselect/.match(mailbox_info.last) # mailbox isn't selectable
				raise NotSelectableMailboxError.new("Can't select #{name} : not a selectable mailbox")
			end

			data = Hash.new
			data['heliotrope-client'] = @heliotropeclient
			data['UID_store'] = @UID_store
			return HeliotropeFakeMailbox.new(self, name, data)
    end

    def delete_mail(mailbox, seqno)
			puts "; trying to delete mails"
			ret = mailbox.delete_seqno(seqno)
			ret
    end

		def copy_mails_to_mailbox(mails, mailbox)
			out = []

			mailbox_name = mailbox.name
			format_label_to_imap!(mailbox_name)
			all_mailboxes = mailboxes
			raise MailboxExistError.new("[TRYCREATE] #{mailbox_name} doesn't exist") if
		 		all_mailboxes.assoc(mailbox_name).nil? 
			raise NotSelectableMailboxError.new("#{mailbox_name} is not selectable") if
				all_mailboxes.assoc(mailbox_name).include?("\\Noselect")

			puts "copy mails to #{mailbox_name}"

			dst_mailbox = get_mailbox(mailbox_name)

			hlabel = format_label_from_imap_to_heliotrope!(mailbox_name)
			mails.each do |m|
				response = dst_mailbox.append_mail(m)
				out << response[:uid]
			end

			out
		end


		def append_mail(message, mailbox_name, flags)
			all_mailboxes = mailboxes
			format_label_to_imap! mailbox_name
			raise MailboxExistError.new("[TRYCREATE] #{mailbox_name} doesn't exist") if
		 		all_mailboxes.assoc(mailbox_name).nil?

			get_mailbox(mailbox_name).append_mail_to_mailbox message, flags
		end

    def open_backend(*args, &block)
      synchronize do
        @backend.open(*args, &block)
      end
    end

    def rebuild_index
      @logger.info("rebuilding index...")
      if @config["delete_ml_mailboxes"]
        @mailbox_db.transaction do
          for k, v in @mailbox_db["mailing_lists"]
            delete_mailbox_internal(v["mailbox"]) if v.key?("mailbox")
          end
          @mailbox_db["mailing_lists"].clear
          delete_mailbox_internal("ml")
          @mailbox_db["mailboxes"]["ml"] = DEFAULT_MAILBOXES["ml"]
        end
      end
      @backend.rebuild_index do
        mailbox_names = {}
        @mailbox_db.transaction do
          for mailbox_name, mailbox_data in @mailbox_db["mailboxes"]
            id = mailbox_data["id"]
            if id
              mailbox_names[id] = mailbox_name
            end
          end
        end
        open_backend do
          mail_dir = File.expand_path("mails", @path)
          Dir.glob(mail_dir + "/*/*").sort.each do |dir|
            reindex_month(dir)
          end
        end
      end
      @uidvalidity_seq.next
      @logger.info("rebuilt index")
    end

    def get_next_mailbox_id
      return @mailbox_id_seq.next
    end



# TODO : those 2 methods fetch the whole wessage, but we don't need the
# body. Maybe add a "complete" arg to the fetch method to fetch only the
# messageinfos ?
		def fetch_labels_and_flags_for_message_id(message_id)
			out = []
			messageinfos = 	@heliotropeclient.message(message_id)
			messageinfos.fetch("labels").each do |l|
				out << "~#{l}" 
			end

			messageinfos.fetch("state").each do |s|
				out << SPECIAL_MAILBOXES.key(s)
			end

			out << "\\Seen" unless out.include?("~unread")
			out.compact.uniq
		end

		def set_labels_and_flags_for_message_id(message_id, flags)
			messageinfos = @heliotropeclient.message(message_id)
			thread_id = messageinfos["thread_id"]
			message_id = messageinfos["message_id"]

			flags.map! do |f|
				format_label_from_imap_to_heliotrope!(f)
			end.compact!

			# separate flags between labels and state
			state = flags.select{|f| MESSAGE_STATE.member?(f)}
			labels = flags - state

			@heliotropeclient.set_labels! thread_id, labels
			@heliotropeclient.set_state! message_id, state

			@heliotropeclient.message message_id
		end

		def fetch_date_for_uid(uid)
			Time.at(@heliotropeclient.message(uid).fetch("date"))
		end

		def next_uid key; @uid_store.member?(key) ?  Marshal.load(@uid_store[key]).to_i : 1 end
		def increment_next_uid key, value; @uid_store[key] = Marshal.dump(value.to_i) end
		def get_uids key; @uid_store.member?(key) ? Marshal.load(@uid_store[key]).to_hash : {} end
		def write_uids key, value; @uid_store[key] = Marshal.dump(value.to_hash) end

		private

		def format_label_to_imap!(label)
			unless /^\~/.match(label) or SPECIAL_MAILBOXES.key?(label)
				raise NoMailboxError.new("#{label} doesn't exist or is invalid")
			end
		end


		def format_label_from_imap_to_heliotrope!(ilabel)
			if SPECIAL_MAILBOXES.key?(ilabel)
				return SPECIAL_MAILBOXES.fetch(ilabel)
			elsif /^\\/.match(ilabel)
				raise NoMailboxError.new("#{ilabel} is not a valid name!")
			else
				return ilabel.gsub(/^\~/,"") # remove ~ from the beginning of the label
			end
		end

    def extract_query(mailbox_name)
      s = mailbox_name.slice(/\Aqueries\/(.*)/u, 1)
      return nil if s.nil?
      query = Net::IMAP.decode_utf7(s)
      begin
        open_backend do |backend|
          result = backend.try_query(query)
        end
        return query
      rescue
        raise InvalidQueryError.new("invalid query")
      end
    end

    def get_next_uid
      return @uid_seq.next
    end

    def extract_body(mail)
      if mail.multipart?
        return mail.body.collect { |part|
          extract_body(part)
        }.join("\n")
      else
        case mail.header.content_type("text/plain")
        when "text/plain"
          return decode_body(mail)
        when "text/html", "text/xml"
          return decode_body(mail).gsub(/<.*?>/um, "")
        else
          return ""
        end
      end
    end

    def decode_body(mail)
      charset = mail.header.params("content-type", {})["charset"] ||
        @default_charset
      return to_utf8(mail.decode, charset)
    end

    def to_utf8(src, charset)
      begin
        return Iconv.conv("utf-8", charset, src)
      rescue
        return NKF.nkf("-m0 -w", src)
      end
    end

    def guess_list_name(fname, mail)
      ret = nil

      if mail.header.include?(fname)
        value = mail.header[fname]
        ret = value
        case fname
        when 'list-id'
          ret = $1 if /<([^<>]+)>/ =~ value
        when 'x-ml-address'
          ret = $& if /\b[^<>@\s]+@[^<>@\s]+\b/ =~ value
        when 'x-mailing-list', 'mailing-list'
          ret = $1 if /<([^<>@]+@[^<>@]+)>/ =~ value
        when 'x-ml-name'
          # noop
        when 'sender'
          if /\bowner-([^<>@\s]+@[^<>@\s]+)\b/ =~ value
            ret = $1
          else
            ret = nil
          end
        when 'x-loop'
          ret = $& if /\b[^<>@\s]+@[^<>@\s]+\b/ =~ value
        end
      end

      ret
    end

    def extract_properties(mail, properties, override)
      for field in ["subject", "from", "to", "cc", "bcc"]
        properties[field] = get_header_field(mail, field)
      end
      begin
        properties["date"] = DateTime.parse(mail.header["date"].to_s).to_time.getlocal.strftime("%Y-%m-%dT%H:%M:%S")
      rescue Exception => e
        @logger.log_exception(e, "failed to parse date - `#{mail.header["date"].to_s}'", Logger::WARN)
      end
      s = nil
      @ml_header_fields.each do |field_name|
        s = guess_list_name(field_name, mail)
        break if s
      end
      properties["x-ml-name"] = decode_encoded_word(s.to_s)
      properties["x-mail-count"] = mail.header["x-mail-count"].to_s.to_i
      return properties.merge(override)
    end

    def get_header_field(mail, field)
      return decode_encoded_word(mail.header[field].to_s)
    end

    def decode_encoded_word(s)
      return NKF.nkf("-w", s)
    end
  end
end
