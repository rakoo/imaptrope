# encoding: UTF-8

class Ximapd
class Person
  def initialize name, email, handle
    @name = name
    @email = email
    @handle = handle
  end

  attr_reader :name, :email, :handle

  def to_email_address
    qname = name =~ /"/ ? name.inspect : name
    [qname, "<#{email}>"].compact.join(" ")
  end

  def display_name; name || handle || email end

  ## takes a string, returns a [name, email, emailnodomain] combo
  ## e.g. for William Morgan <wmorgan@example.com>, returns
  ##  ["William Morgan", wmorgan@example.com, wmorgan]
  def self.from_string string # ripped from sup
    return if string.nil? || string.empty?

    name, email, handle = case string
    when /^(["'])(.*?[^\\])\1\s*<((\S+?)@\S+?)>/
      a, b, c = $2, $3, $4
      a = a.gsub(/\\(["'])/, '\1')
      [a, b, c]
    when /(.+?)\s*<((\S+?)@\S+?)>/
      [$1, $2, $3]
    when /<((\S+?)@\S+?)>/
      [nil, $1, $2]
    when /((\S+?)@\S+)/
      [nil, $1, $2]
    else
      [nil, string, nil] # i guess...
    end

    Person.new name, email, handle
  end

  def self.many_from_string string
    return [] if string.nil? || string !~ /\S/
    emails = string.gsub(/[\t\r\n]+/, " ").split(/,\s*(?=(?:[^"]*"[^"]*")*(?![^"]*"))/)
    emails.map { |e| from_string e }.compact
  end

  def indexable_text; [name, email, handle].join(" ") end

	def domain
# Pray that the domain really is the last part, for example 
# foo@bar@domain
# is in the domain "domain"
		email.gsub(/.*\@/,"") 
	end

	def mailbox
		@handle
	end

	def empty?
	if @name.empty? or @name.nil? or @email.empty? or @email.nil? or @handle.empty? or @handle.nil?
		return true
	else
		return false
	end
	end


end
end
