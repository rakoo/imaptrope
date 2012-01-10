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

class IMAPTrope
  module DoubleDispatchable
    def self.append_features(klass)
      super(klass)

      klass.class_eval do
        @double_dispatch_methods = {}
        @double_dispatched_methods = {}

        class << self
          attr_reader :double_dispatch_methods
          attr_reader :double_dispatched_methods
        end
      end

      class << klass
        self
      end.send(:define_method, :double_dispatch) do |method, prefix|
        klass.double_dispatch_methods[method] = prefix
        @double_dispatched_methods[prefix] = []
        define_method(method) do
          raise SubclassResponsibilityError.new
        end
      end

      class << klass
        self
      end.send(:define_method, :inherited) do |subclass|
        klass.double_dispatch_methods.each do |method, prefix|
          m = prefix.to_s +
            subclass.name.slice(/[A-Za-z]+\z/).gsub(/[A-Z]/) { |s|
            "_" + s.downcase
          }
          mid = m.intern
          subclass.send(:define_method, method) do |obj, *args|
            obj.send(mid, self, *args)
          end
          klass.double_dispatched_methods[prefix].push(mid)
        end
      end
    end
  end
end
