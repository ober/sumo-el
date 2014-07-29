#!/usr/bin/env ruby
require 'sinatra'
require 'pp'

post '/foo' do
  output = ""
  if request.body.class != StringIO
    output << "WAS NIL!!! #{request.body.class.to_s}"
    puts "WAS NIL!!! #{request.body.class.to_s}"
  else
    puts request.body.string
    puts request.media_type
  end
  return output
end
