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

class Ximapd
  DEFAULT_CHARSET = "iso-2022-jp"
  DEFAULT_ML_HEADER_FIELDS = [
    "x-ml-name",
    "list-id",
    "mailing-list"
  ]
  DEFAULT_MAILBOXES = {
    "ml" => {
      "flags" => "\\Noselect"
    },
    "queries" => {
      "flags" => "\\Noselect"
    },
    "static" => {
      "flags" => "\\Noselect"
    }
  }

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

#MailboxStatus = Struct.new(:messages, :recent, :uidnext, :uidvalidity,
#:unseen)

  class MailStore
    include MonitorMixin

		MESSAGE_MUTABLE_STATE = Set.new %w(starred unread deleted)
		MESSAGE_IMMUTABLE_STATE = Set.new %w(attachment signed encrypted draft sent)
		MESSAGE_STATE = MESSAGE_IMMUTABLE_STATE + MESSAGE_MUTABLE_STATE
		SPECIAL_MAILBOXES = Hash[
			"\\Seen" => nil,
			"\\Answered" => nil,
			"\\Flagged" => nil,
			"\\Starred" => "~starred",
		 	"\\Draft" => "~draft",
		 	"\\Deleted" => "~deleted",
		 	"All Mail" => nil,
		 	"INBOX" => "~inbox"
		]
		# misses \Answered \Flagged \Seen (the latter is problematic)
		# this Hash relates IMAP keywords with their Heliotrope
		# counterparts

		attr_reader :heliotropeclient
    attr_reader :config, :path, :mailbox_db, :mailbox_db_path
    attr_reader :plugins
    attr_reader :uid_seq, :uidvalidity_seq, :mailbox_id_seq
#attr_reader :backend

    def initialize(config)
      super()
      @config = config
      @logger = @config["logger"]
			@heliotropeclient = HeliotropeClient.new "http://localhost:8043"
# Override evrything, we only want it to work with heliotrope
#@backend = HeliotropeBackend.new




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
			labels = @heliotropeclient.labels

			#Format to return: 
			#[
			# ["label1", "FLAGS for label1"],
			# ["label2", "FLAGS for label2"],
			#]

			out = []
			labels.each do |label|
				if MESSAGE_IMMUTABLE_STATE.member?(label)
					next
				elsif SPECIAL_MAILBOXES.value?(label)
					out << [SPECIAL_MAILBOXES.key(label), ""] # use the IMAP special name, not the heliotrope one
				else
				out << ["\~"+label, ""]
				end
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




    def get_mailbox_status(mailbox_name, read_only)

			format_label_to_imap!(mailbox_name)
			mailbox = get_mailbox mailbox_name
			mailbox_status = mailbox.status
			mailbox_status.uidvalidity = @uidvalidity_seq.current
			#unless read_only
				#@last_peeked_uids[mailbox_name] = @uid_seq.current.to_i
			#end

			if mailbox_name == "All Mail"
				mailbox_status.messages = @heliotropeclient.size
				mailbox_status.unseen = @heliotropeclient.count "~unread"
			elsif SPECIAL_MAILBOXES.member?(mailbox_name)
				mailbox_status.messages = @heliotropeclient.count "#{SPECIAL_MAILBOXES[mailbox_name]}"
				mailbox_status.unseen = @heliotropeclient.count "~unread+#{SPECIAL_MAILBOXES[mailbox_name]}"
			else
				mailbox_status.messages = @heliotropeclient.count "#{mailbox_name}"
				mailbox_status.unseen = @heliotropeclient.count "~unread+#{mailbox_name}"
			end


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
			return HeliotropeFakeMailbox.new(self, name, data)

      #if name == "DEFAULT"
        #return DefaultMailbox.new(self)
      #end
      #data = @mailbox_db["mailboxes"][name]
      #unless data
        #raise NoMailboxError.new("no such mailbox")
      #end
      #class_name = data["class"]
      #if class_name
        #return Ximapd.const_get(class_name).new(self, name,
                                                #@mailbox_db["mailboxes"][name])
      #else
        #return SearchBasedMailbox.new(self, name,
                                      #@mailbox_db["mailboxes"][name])
      #end
    end

    def delete_mails(mails)
			puts "trying to delete mails; I will put the label \"~deleted\" on them"
			for mail in mails
				thread_id = @heliotropeclient.message(mail.uid)["thread_id"]
				@heliotropeclient.set_labels! thread_id, ["~deleted"]
			end
    end

		def copy_mails_to_mailbox(mails, mailbox)
			mailbox_name = mailbox.name
			format_label_to_imap!(mailbox_name)
			all_mailboxes = mailboxes
			raise MailboxExistError.new("[TRYCREATE] #{mailbox_name} doesn't exist") if all_mailboxes.assoc(mailbox_name).nil? 
			raise NotSelectableMailboxError.new("#{mailbox_name} is not selectable") if all_mailboxes.assoc(mailbox_name).include?("\\Noselect")

			puts "copy mails to #{mailbox_name}"

			out = []

			hlabel = format_label_from_imap_to_heliotrope!(mailbox_name)
			mails.each do |m|
				new_labels = m.labels.split(' ') + [hlabel]
				uid = m.uid
				out << uid
				set_labels_and_flags_for_uid(uid, new_labels)
			end

			out
		end


		def append_mail(message, mailbox_name, flags)
			all_mailboxes = mailboxes
			format_label_to_imap! mailbox_name
			raise MailboxExistError.new("[TRYCREATE] #{mailbox_name} doesn't exist") if all_mailboxes.assoc(mailbox_name).nil?

			state = (Set.new(flags) & MESSAGE_MUTABLE_STATE).to_a.compact
			state.map!{ |f| format_label_from_imap_to_heliotrope!(f) }.compact.uniq!

			flags = (Set.new(flags) - MESSAGE_MUTABLE_STATE).to_a.compact
			flags.map! { |f| format_label_from_imap_to_heliotrope!(f) }

			hlabel = format_label_from_imap_to_heliotrope!(mailbox_name)
			flags = (flags + [hlabel]).compact.uniq

			#construct the message body. We need a message_id
			# not trusted yet
			#validated_message = Message.validate(message)
			
			message = message.force_encoding("binary") if message.respond_to?(:force_encoding)

			@heliotropeclient.add_message(message, :labels => flags, :state => state)
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

# methods to retrieve informations of a message from within it
# heliotrope doesn't really have mailboxes, so these functions really
# belong here. 

		def fetch_rawbody_for_uid(uid)
			@heliotropeclient.raw_message uid
		end

# TODO : those 2 methods fetch the whole wessage, but we don't need the
# body. Maybe add a "complete" arg to the fetch method to fetch only the
# messageinfos ?
		def fetch_labels_and_flags_for_uid(uid)
			out = []
			@heliotropeclient.message(uid).fetch("labels").each do |l|
				out << "~#{l}" 
			end
			out << "\\Seen" unless out.include?("~unread")
			out.compact
		end

		def set_labels_and_flags_for_uid(uid, flags)
			thread_id = @heliotropeclient.message(uid)["thread_id"]
			flags.map! do |f|
				format_label_from_imap_to_heliotrope!(f)
			end.compact!
			@heliotropeclient.set_labels! thread_id, flags
		end

		def fetch_date_for_uid(uid)
			Time.at(@heliotropeclient.message(uid).fetch("date"))
		end

		def fetch_mails(sequence_set)
			mails = []
			sequence_set.each do |atom|
				case atom
				when Range
					atom.each do |uid|
						messageinfos = @heliotropeclient.message uid
						mails.push(Message.new(messageinfos, self))
					end
				else
					messageinfos = @heliotropeclient.message atom
					mails.push(Message.new(messageinfos, self))
				end
			end
			mails
		end

		def format(query)
			queryterms = query.to_s.split("+")

			out = queryterms.map! do |term|
				if SPECIAL_MAILBOXES.include?(term)
					SPECIAL_MAILBOXES.fetch(term)
				end
			end.join("+")

			if @name != "All Mail"
					out << "#{@name}" << "+"<< out
			end

			CGI.unescape(out)
		end


    private

		def format_label_to_imap!(label)
			unless /^\~/.match(label) or SPECIAL_MAILBOXES.key?(label)
				raise NoMailboxError.new("Don't forget the ~ before the name !")
			end
		end


		def format_label_from_imap_to_heliotrope!(ilabel)
			if SPECIAL_MAILBOXES.key?(ilabel)
				return SPECIAL_MAILBOXES.fetch(ilabel)
			else
				return ilabel.gsub(/^\~/,"") # remove ~ from the beginning of the label
			end
		end

    def override_commit_new(db)
      def db.commit_new(f)
        f.truncate(0)
        f.rewind
        new_file = @filename + ".new"
        File.open(new_file) do |nf|
          nf.binmode
          FileUtils.copy_stream(nf, f)
        end
        f.fsync
        File.unlink(new_file)
      end
    end

    def convert_old_mailbox_db
      if @mailbox_db.root?("status")
        @uid_seq.current = @mailbox_db["status"]["last_uid"]
        @uidvalidity_seq.current = @mailbox_db["status"]["uidvalidity"]
        @mailbox_id_seq.current = @mailbox_db["status"]["last_mailbox_id"]
        @mailbox_db.delete("status")
      end
      if @mailbox_db.root?("mailing-lists")
        for key, val in @mailbox_db["mailing-lists"]
          @mailbox_db["mailing_lists"][key] = {
            "creator_uid" => val
          }
        end
        @mailbox_db.delete("mailing-lists")
      end
      @mailbox_db["mailboxes"]["static"] ||= {
        "flags" => "\\Noselect"
      }
    end

    def strip_unix_from(str, indate)
      str.sub!(/\AFrom\s+\S+\s+(.*)\r\n/) do
        if indate.nil?
          begin
            indate = DateTime.strptime($1 + " " + TIMEZONE,
                                       "%a %b %d %H:%M:%S %Y %z")
          rescue
          end
        end
        ""
      end
      return indate
    end

    def mkdir_p(dirname)
      if /\A\//n.match(dirname)
        raise "can't specify absolute path"
      end
      if dirname == "." ||
        @mailbox_db["mailboxes"].include?(dirname)
        return
      end
      mkdir_p(File.dirname(dirname))
      @mailbox_db["mailboxes"][dirname] = {
        "flags" => "\\Noselect"
      }
    end

    def import_mail_internal(str, mailbox_name = nil, flags = "", indate = nil, override = {})
      uid = get_next_uid
      mail = parse_mail(str, uid, flags, indate, override)
      @mailbox_db.transaction do
        if mailbox_name
          mailbox = get_mailbox(mailbox_name)
        else
          mailbox = nil
          for plugin in @plugins
            begin
              mbox_name = plugin.filter(mail)
              if mbox_name == "REJECT"
                @logger.add(Logger::INFO, "rejected: from=<#{mail.properties['from']}> subject=<#{mail.properties['subject'].gsub(/[ \t]*\n[ \t]+/, ' ')}> date=<#{mail.properties['date']}>")
                return 0
              end
              if mbox_name
                mailbox = get_mailbox(mbox_name)
                break
              end
            rescue Exception => e
              @logger.log_exception(e)
            end
          end
          mailbox ||= DefaultMailbox.new(self)
        end
        mailbox.import(mail)
        @logger.add(Logger::INFO, "imported: uid=#{mail.uid} from=<#{mail.properties['from']}> subject=<#{mail.properties['subject'].gsub(/[ \t]*\n[ \t]+/, ' ')}> date=<#{mail.properties['date']}> mailbox=<#{mailbox.name}>")
      end
      return mail.uid
    end

    def get_mailbox_name_from_x_ml_name(s)
      mbox = s.slice(/(.*) <.*>/u, 1) 
      if mbox.nil?
        mbox = s.slice(/<(.*)>/u, 1) 
        if mbox.nil?
          mbox = s.slice(/\S+@[^ \t;]+/u) 
          if mbox.nil?
            mbox = s
          end
        end
      end
      return mbox
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





    def reindex_month(dir)
      filenames = []
      Find.find(dir) do |filename|
        if File.file?(filename) && /\/\d+\z/.match(filename)
          filenames.push(filename)
        end
      end
      if @config["progress"]
        month = dir.slice(/\d+\/\d+\z/)
        progress_bar = ProgressBar.new(month, filenames.length)
      else
        progress_bar = NullObject.new
      end
      for filename in filenames
        reindex_mail(filename)
        progress_bar.inc
      end
      progress_bar.finish
    end

    def reindex_mail(filename)
      begin
        str = File.read(filename)
        uid = filename.slice(/\/(\d+)\z/, 1).to_i
        flags = @backend.get_old_flags(uid) || "\\Seen"
        indate = File.mtime(filename)
        mail = parse_mail(str, uid, flags, indate)
        begin
          mail.properties["mailbox-id"] =
            File.read(filename + ".mailbox-id").to_i
        rescue Errno::ENOENT
        end
        @mailbox_db.transaction do
          index_mail(mail, filename)
        end
      rescue StandardError => e
        @logger.log_exception(e)
      end
    end

    def parse_mail(mail, uid, flags, indate, override = {})
      mail.gsub!(/\r?\n/, "\r\n")
      indate = strip_unix_from(mail, indate)
      if indate
        indate = indate.to_time.getlocal.to_datetime
      else
        indate = DateTime.now
      end
      properties = Hash.new("")
      properties["uid"] = uid
      properties["size"] = mail.size
      properties["flags"] = ""
      properties["internal-date"] = indate.to_time.getlocal.strftime("%Y-%m-%dT%H:%M:%S")
      properties["date"] = properties["internal-date"]
      properties["x-mail-count"] = 0
      properties["mailbox-id"] = 0
      begin
        m = @mail_parser.parse(mail.gsub(/\r\n/, "\n"))
        properties = extract_properties(m, properties, override)
        body = extract_body(m)
      rescue Exception => e
        @logger.log_exception(e, "failed to parse mail uid=#{uid}",
                              Logger::WARN)
        header, body = *mail.split(/^\r\n/)
        body = to_utf8(body, @default_charset)
      end
      return MailData.new(mail, uid, flags, indate, body, properties,
                          m || NullMessage.new)
    end

    def get_mailbox_id(mailbox_name)
      if mailbox_name.nil?
        mailbox_id = 0
      else
        mailbox = @mailbox_db["mailboxes"][mailbox_name]
        if mailbox.nil?
          raise NoMailboxError.new("no such mailbox")
        end
        mailbox_id = mailbox["id"]
        if mailbox_id.nil?
          raise MailboxAccessError.new("can't import to mailbox without id")
        end
      end
      return mailbox_id
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
