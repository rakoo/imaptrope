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

class Expirer < Plugin
  def on_idle
    expired_mailboxes = Set.new
    for pattern, limit in @config["expires"]
      re = Regexp.new(pattern, nil, "u")
      mboxes = @mail_store.mailbox_db["mailboxes"].keys.select { |mailbox_name|
        re.match(mailbox_name) && !expired_mailboxes.include?(mailbox_name)
      }
      for mailbox_name in mboxes
        mailbox = @mail_store.get_mailbox(mailbox_name)
        count = mailbox.expire_by_date(limit["days"])
        if count > 0
          @logger.info("expired #{count} mails in #{mailbox_name}")
        end
        expired_mailboxes.add(mailbox_name)
      end
    end
  end
end

class Mailbox
  def expire_by_date(days)
    return 0
  end
end

class StaticMailbox
  def expire_by_date(days)
    limit_date = Time.now - days * 24 * 60 * 60
    uids = get_uids
    count = 0
    for uid in uids
      mail = StaticMail.new(@config, self, uid, uid)
      break if mail.internal_date >= limit_date
      if /\\Seen\b/ni.match(mail.flags(false))
        mail.delete
        count += 1
      end
    end
    return count
  end
end
