# encoding: UTF-8

require 'rmail'
require 'digest/md5'
require 'json'
require 'timeout'

class Ximapd
class InvalidMessageError < StandardError; end
class Message
  def initialize messageinfos, mail_store
		@messageinfos = messageinfos
		@mail_store = mail_store
    @mime_parts = {}

# These fields shouldn't change, so it is safe to have them once and for
# all

		@msgid = messageinfos["email_message_id"]
    @safe_msgid = munge_msgid @msgid

		@from = Person.from_string messageinfos["from"]
		@date = messageinfos["date"] #careful : this is UNIX time

		@to = Person.many_from_string messageinfos["to"].join(",")
		@cc = Person.many_from_string messageinfos["cc"].join(",")
		@bcc = Person.many_from_string messageinfos["bcc"].join(",")
		@subject = messageinfos["subject"]
		@reply_to = Person.from_string messageinfos["reply_to"]
		@refs = messageinfos["refs"]
    @safe_refs = @refs.map { |r| munge_msgid(r) }


		@recipient_email = messageinfos["recipient_email"]
		@list_subscribe = messageinfos["list_subscribe"]
		@list_unsubscribe = messageinfos["list_unsubscribe"]
		@list_post = messageinfos["list_post"]

		@mime_parts = messageinfos["parts"]

# but labels can change, so we should ALWAYS fetch for them

# These are internal, but can help
		@uid = messageinfos["message_id"]
		@thread_id = messageinfos["thread_id"] # we should check for it, because it can also change


    #@m = RMail::Parser.read @rawbody

    #@msgid = find_msgids(decode_header(validate_field(:message_id, @m.header["message-id"]))).first
    ### this next error happens if we have a field, but we can't find a <something> in it
    #raise InvalidMessageError, "can't parse msgid: #{@m.header['message-id']}" unless @msgid
    #@safe_msgid = munge_msgid @msgid

    #@from = Person.from_string decode_header(validate_field(:from, @m.header["from"]))
    #@date = begin
      #Time.parse(validate_field(:date, @m.header["date"])).to_i
    #rescue ArgumentError
      #puts "warning: invalid date field #{@m.header['date']}"
      #0
    #end

    #@to = Person.many_from_string decode_header(@m.header["to"])
    #@cc = Person.many_from_string decode_header(@m.header["cc"])
    #@bcc = Person.many_from_string decode_header(@m.header["bcc"])
    #@subject = decode_header @m.header["subject"]
    #@reply_to = Person.from_string @m.header["reply-to"]

    #@refs = find_msgids decode_header(@m.header["references"] || "")
    #in_reply_to = find_msgids decode_header(@m.header["in-reply-to"] || "")
    #@refs += in_reply_to unless @refs.member? in_reply_to.first
    #@safe_refs = @refs.map { |r| munge_msgid(r) }

    ### various other headers that you don't think we will need until we
    ### actually need them.

    ### this is sometimes useful for determining who was the actual target of
    ### the email, in the case that someone has aliases
    #@recipient_email = @m.header["envelope-to"] || @m.header["x-original-to"] || @m.header["delivered-to"]

    #@list_subscribe = @m.header["list-subscribe"]
    #@list_unsubscribe = @m.header["list-unsubscribe"]
    #@list_post = @m.header["list-post"] || @m.header["x-mailing-list"]

    self
  end

  attr_reader :msgid, :from, :to, :cc, :bcc, :subject, :date, :refs, :recipient_email, :list_post, :list_unsubscribe, :list_subscribe, :reply_to, :safe_msgid, :safe_refs

# aliases for IMAP
	attr_reader :uid
	def seqno; @uid end

	def flags(get_recent=true)
		# get flags AND labels
 		@mail_store.fetch_labels_and_flags_for_uid uid 
	end

	def internal_date
		Time.at(@date)
	end

	def size
		# TODO: verify if this is really conform with RFC2822
		# Get the rawbody only at that time, because some people still send
		# gigabytes of sh*t through emails, so it can be very large.
		@mail_store.fetch_rawbody_for_uid(@uid).bytesize
	end

	def envelope
		out = "("
		out << quoted(Time.at(@date).to_s) << " "
		out << quoted(@subject) << " "
		out << envelope_addrs(@from) << " "
		out << envelope_addrs(@from) << " " # should be sender
		out << envelope_addrs(@reply_to) << " "
		out << envelope_addrs(@to) << " "
		out << envelope_addrs(@cc) << " "
		out << envelope_addrs(@bcc)  << " "
		out << "NIL" << " " # should be in-reply-to, but this header isn't known : TODO
		out << quoted("<" + @msgid + ">")
		out << ")"
			
		out
	end

	def get_header(part=nil)
# I don't know about this one, should we display only the message
# header, or the header for each part ? For the moment, I'm being lazy

		if part
			return get_part(part).body.to_s.slice(/.*?\n\n/mn).gsub(/\n/, "\r\n")
		else
			@mail_store.fetch_rawbody_for_uid(@uid).split(/\n\n/).first + "\n\n"
		end
	end

	def body
		rawbody = @mail_store.fetch_rawbody_for_uid(@uid).split(/\n\n/)
		rawbody.shift
		rawbody.join("\n\n") + "\n\n"
	end
		


	def get_header_fields(fields, part = nil)
# Get some of the headers, those specified by fields
		pat = "^(?:" + fields.collect { |field|
			Regexp.quote(field)
		}.join("|") + "):.*(?:\n[ \t]+.*)*\n"
		re = Regexp.new(pat, true, "n")
		return get_header(part).scan(re).join + "\r\n"
	end


	def body_structure(extensible)
		return body_structure_internal(parsed_mail, extensible)
	end


  def direct_recipients; to end
  def indirect_recipients; cc + bcc end
  def recipients; direct_recipients + indirect_recipients end

  def indexable_text
    @indexable_text ||= begin
      v = ([from.indexable_text] +
        recipients.map { |r| r.indexable_text } +
        [subject] +
        mime_parts("text/plain").map do |type, fn, id, content|
          if fn
            fn
          elsif type =~ /text\//
            content
          end
        end
      ).flatten.compact.join(" ")

      v.gsub(/\s+[\W\d_]+(\s|$)/, " "). # drop funny tokens
        gsub(/\s+/, " ")
    end
  end

  SIGNED_MIME_TYPE = %r{multipart/signed;.*protocol="?application/pgp-signature"?}m
  ENCRYPTED_MIME_TYPE = %r{multipart/encrypted;.*protocol="?application/pgp-encrypted"?}m
  SIGNATURE_ATTACHMENT_TYPE = %r{application\/pgp-signature\b}

  #def snippet
    #mime_parts("text/plain").each do |type, fn, id, content|
      #if (type =~ /text\//) && fn.nil?
        #head = content[0, 1000].split "\n"
        #head.shift while !head.empty? && head.first.empty? || head.first =~ /^\s*>|\-\-\-|(wrote|said):\s*$/
        #snippet = head.join(" ").gsub(/^\s+/, "").gsub(/\s+/, " ")[0, 100]
        #return snippet
      #end
    #end
    #""
  #end

  def has_attachment?
    @has_attachment ||=
      mime_parts("text/plain").any? do |type, fn, id, content|
        fn && (type !~ SIGNATURE_ATTACHMENT_TYPE)
    end
  end

  def signed?
    @signed ||= mime_part_types.any? { |t| t =~ SIGNED_MIME_TYPE }
  end

  def encrypted?
    @encrypted ||= mime_part_types.any? { |t| t =~ ENCRYPTED_MIME_TYPE }
  end

  def mime_parts preferred_type
    @mime_parts[preferred_type] 
  end

	def multipart?
		mail = parsed_mail
		return mail.multipart?
	end

	def mime_header(part)
		return get_part(part).header.to_s + "\n"
	end

	def mime_body(part)
		if part.nil?
			return to_s
		else
			return get_part(part).body.to_s
		end
	end

	def to_s
		@mail_store.fetch_rawbody_for_uid @uid
	end


private

	def get_part(part)
		part_numbers = part.split(/\./).collect { |i| i.to_i - 1 }
		return get_part_internal(parsed_mail, part_numbers)
	end

	def get_part_internal(mail, part_numbers)
		n = part_numbers.shift
		if /message\/rfc822/n.match(mail.header.content_type)
			mail = RMail::Parser.read(mail.body)
		end
		if !mail.multipart? && n == 0
			m = mail
		else
			m = mail.part(n)
		end
		if part_numbers.empty?
			return m
		end
		return get_part_internal(m, part_numbers)
	end


	def envelope_addrs(addrs)
		if addrs.nil? || addrs.empty?
			return "NIL"
		else
			case addrs
			when Array
				return "(" + addrs.collect { |addr|
					envelope_addr(addr)
				}.join(" ") + ")"
			else #addrs is a Person
				return "(" + envelope_addr(addrs) + ")"
			end
		end
	end

	def envelope_addr(addr)
		name = addr.display_name
# TODO : proper stuff
		adl = nil # appears to be useless
		mailbox = addr.mailbox
		host = addr.domain
		return format("(%s %s %s %s)",
									quoted(name), quoted(adl), quoted(mailbox), quoted(host))
	end

	def body_structure_internal(mail, extensible)
		ary = []
		if /message\/rfc822/n.match(mail.header["content_type"])
			body = RMail::Parser.read(mail.body)
			ary.push(quoted("MESSAGE"))
			ary.push(quoted("RFC822"))
			ary.push(body_fields(mail, extensible))
#ary.push(envelope_internal(body))
			ary.push(body_structure_internal(body, extensible))
			ary.push(mail.body.to_a.length.to_s)
		elsif mail.multipart?
			parts = mail.body.collect { |part|
				body_structure_internal(part, extensible)
			}.join
			ary.push(parts)
			ary.push(quoted(upcase(mail.header.subtype)))
			if extensible
				ary.push(body_ext_mpart(mail))
			end
		else
			ary.push(quoted(upcase(mail.header.media_type)))
			ary.push(quoted(upcase(mail.header.subtype)))
			ary.push(body_fields(mail, extensible))
			if mail.header.media_type == "text"
				ary.push(mail.body.split(/\n/).length.to_s)
			end
			if extensible
				ary.push(body_ext_1part(mail))
			end
		end
		return "(" + ary.join(" ") + ")"
	end

	def body_fields(mail, extensible)
		fields = []
		params = "(" + mail.header.params("content-type", {}).collect { |k, v|
			v.gsub!(/\s/,"")
			format("%s %s", quoted(upcase(k)), quoted(v))
		}.join(" ") + ")"
		if params == "()"
			fields.push("NIL")
		else
			fields.push(params)
		end
		fields.push("NIL")
		fields.push("NIL")
		content_transfer_encoding =
			(mail.header["content-transfer-encoding"] || "7BIT").to_s.upcase
		fields.push(quoted(content_transfer_encoding))
		fields.push(mail.body.gsub(/\n/, "\r\n").length.to_s)
		return fields.join(" ")
	end

  ## hash the fuck out of all message ids. trust me, you want this.
  def munge_msgid msgid
    Digest::MD5.hexdigest msgid
  end

  def find_msgids msgids
    msgids.scan(/<(.+?)>/).map(&:first)
  end

  def mime_part_types part=@m
    ptype = part.header["content-type"] || ""
    [ptype] + (part.multipart? ? part.body.map { |sub| mime_part_types sub } : [])
  end

  ## unnests all the mime stuff and returns a list of [type, filename, content]
  ## tuples.
  ##
  ## for multipart/alternative parts, will only return the subpart that matches
  ## preferred_type. if none of them, will only return the first subpart.
  def decode_mime_parts part, preferred_type, level=0
    if part.multipart?
      if mime_type_for(part) =~ /multipart\/alternative/
        target = part.body.find { |p| mime_type_for(p).index(preferred_type) } || part.body.first
        if target # this can be nil
          decode_mime_parts target, preferred_type, level + 1
        else
          []
        end
      else # decode 'em all
        part.body.compact.map { |subpart| decode_mime_parts subpart, preferred_type, level + 1 }.flatten 1
      end
    else
      type = mime_type_for part
      filename = mime_filename_for part
      id = mime_id_for part
      content = mime_content_for part, preferred_type
      [[type, filename, id, content]]
    end
  end

private

	def parsed_mail
		if @parsed_mail.nil?
			@parsed_mail = RMail::Parser.read(@mail_store.fetch_rawbody_for_uid(@uid))
		end
		return @parsed_mail
	end

	def body_ext_mpart(mail)
		exts = []
		exts.push(body_fld_param(mail))
		exts.push(body_fld_dsp(mail))
		exts.push("NIL")
	end

	def body_ext_1part(mail)
		exts = []
		exts.push("NIL")
		exts.push(body_fld_dsp(mail))
		exts.push("NIL")
		return exts.join(" ")
	end

	def body_fld_param(mail)
		unless mail.header.field?("content-type")
			return "NIL"
		end
		params = mail.header.params("content-type", {}).collect { |k, v|
			v.gsub!(/\s/,"")
			format("%s %s", quoted(upcase(k)), quoted(v))
		}
		if params.empty?
			return "NIL"
		else
			return "(" + params.join(" ") + ")"
		end
	end

	def body_fld_dsp(mail)
		unless mail.header.field?("content-disposition")
			return "NIL"
		end
		params = mail.header.params("content-disposition", {}).collect { |k, v|
			v.gsub!(/\s/,"")
			format("%s %s", quoted(upcase(k)), quoted(v))
		}
		if params.empty?
			p = "NIL"
		else
			p = "(" + params.join(" ") + ")"
		end
		value = mail.header["content-disposition"].sub(/;.*/mn, "")
		return format("(%s %s)", quoted(upcase(value)), p)
	end

	def upcase(s)
		if s.nil?
			return s
		end
		return s.upcase
	end


	def quoted(s)
		s = (s.nil? ? "" : s)
		"\"" << s << "\"" 
	end

  def validate_field what, thing
    raise InvalidMessageError, "missing '#{what}' header" if thing.nil?
    thing = thing.to_s.strip
    raise InvalidMessageError, "blank '#{what}' header: #{thing.inspect}" if thing.empty?
    thing
  end

  def mime_type_for part
    (part.header["content-type"] || "text/plain").gsub(/\s+/, " ").strip.downcase
  end

  def mime_id_for part
    header = part.header["content-id"]
    case header
      when /<(.+?)>/; $1
      else header
    end
  end

  ## a filename, or nil
  def mime_filename_for part
    cd = part.header["Content-Disposition"]
    ct = part.header["Content-Type"]

    ## RFC 2183 (Content-Disposition) specifies that disposition-parms are
    ## separated by ";". So, we match everything up to " and ; (if present).
    filename = if ct && ct =~ /name="?(.*?[^\\])("|;|\z)/im # find in content-type
      $1
    elsif cd && cd =~ /filename="?(.*?[^\\])("|;|\z)/m # find in content-disposition
      $1
    end

    ## filename could be RFC2047 encoded
    decode_header(filename).chomp if filename
  end

  ## rfc2047-decode a header, convert to utf-8, and normalize whitespace
  def decode_header v
    return "" if v.nil?

    v = if Decoder.is_rfc2047_encoded? v
      Decoder.decode_rfc2047 "utf-8", v
    else # assume it's ascii and transcode
      Decoder.transcode "utf-8", "ascii", v
    end

    v.gsub(/\s+/, " ").strip
  end

  CONVERSIONS = {
    ["text/html", "text/plain"] => :html_to_text
  }

  ## the content of a mime part itself. if the content-type is text/*,
  ## it will be converted to utf8. otherwise, it will be left in the
  ## original encoding
  def mime_content_for mime_part, preferred_type
    return "" unless mime_part.body # sometimes this happens. not sure why.

    mt = mime_type_for(mime_part) || "text/plain" # i guess
    content_type = if mt =~ /^(.+);/ then $1.downcase else mt end
    source_charset = if mt =~ /charset="?(.*?)"?(;|$)/i then $1 else "US-ASCII" end

    content = mime_part.decode
    converted_content, converted_charset = if(converter = CONVERSIONS[[content_type, preferred_type]])
      send converter, content, source_charset
    else
      [content, source_charset]
    end

    if content_type =~ /^text\//
      Decoder.transcode "utf-8", converted_charset, converted_content
    else
      converted_content
    end
  end

  require 'locale'
  SYSTEM_CHARSET = Locale.charset
  HTML_CONVERSION_CMD = "html2text"
  HTML_CONVERSION_TIMEOUT = 10 # seconds... this thing can be slow
  def html_to_text html, charset
    ## ignore charset. html2text produces output in the system charset.
    #puts "; forced to decode html. running #{HTML_CONVERSION_CMD} on #{html.size}b mime part..."
    content = begin
      Timeout.timeout(HTML_CONVERSION_TIMEOUT) do
        Heliotrope.popen3(HTML_CONVERSION_CMD) do |inn, out, err|
          inn.print html
          inn.close
          out.read
        end
      end
    rescue Timeout::Error
      $stderr.puts "; warning: timeout when converting message from html to text"
      "[html conversion failed on this command (htmlconversionfailure)]"
    end
    [content, SYSTEM_CHARSET]
  end
end
end