
num_lines = ARGV[0]

File.open('many_indents.py', 'w') { |file| 
  (0..num_lines).each do |n|
    file.write("#{" " * n}#{n}\n")			
  end
}

File.open('many_indents.py.expected', 'w') { |file| 
  (0..num_lines).each do |n|	
    file.write("(LIT #{n})\n")
				file.write("(NEWLINE)\n")
				if n < num_lines then file.write("(INDENT)\n") end
  end
  
  #first line is not indented, so one less dedent than lines
  (1..num_lines).each do |n|
    file.write("(DEDENT)\n")
  end
  
  file.write("(ENDMARKER)\n")
}
