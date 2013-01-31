(0x00..0xFF).each do |num|
  puts "#{ARGV[0]}'\\x#{"%02x" % num}'"
end
