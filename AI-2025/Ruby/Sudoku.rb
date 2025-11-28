
def print_puzzle
  puts "\nPuzzle:"
  $puzzle.each do |row|
    row.each do |val|
      print "#{val} "
    end
    puts ' '
  end
end

def read_file(filename)
  puts "#{filename}"
  afile = File.new(filename, 'r')
  if afile
    content = afile.sysread(300)
    j = 0
    content.each_line do |line|
      # puts "j:#{j}"
      # do something with line
      unless line.start_with? '#'
        # puts "Line: #{line}"
        i = 0
        line.split(' ').each do |val|
          # puts "i:#{i}"
          $puzzle[j][i] = val.to_i
          i += 1
        end
        j += 1
      end
    end
  else
    puts 'Unable to open file!'
  end
end

def is_possible(y,x,val)
  if $DEBUG > 0 then puts "Is possible #{y}, #{x}, #{val}" end
  (0..8).each do |i|
    if $puzzle[i][x] == val then return false end
  end
  (0..8).each do |i|
    if $puzzle[y][i] == val then return false end
  end

  # Search the Rectangle containing x & y
  # Find which 3x3 square we are in using the floor quotient
  x0 = x / 3.floor * 3
  y0 = y / 3.floor * 3
  if $DEBUG > 2 then puts "Is possible y=#{y} y0=#{y0}, x=#{x} x0=#{x0}, val=#{val}" end
  (0..2).each do |i|
    (0..2).each do |j|
      # print(f"y0+i={y0+i} i={i}, x0+j={x0+j} j={j} Puzzle[y0+i][x0+j]={puzzle[y0+i][x0+j]}, val={val}")
      if $puzzle[y0 + i][x0 + j] == val then return false end
    end
  end
  if $DEBUG > 0 then puts "YES Is possible" end
  return true
end

def solve
  (0..8).each do |j|
    (0..8).each do |i|
      if $puzzle[j][i] == 0 then
        if $DEBUG > 0 then puts "Solve: j=#{j},i=#{i}: #{$puzzle[j][i]}" end
        (1..9).each do |val|
          $count += 1
          if $DEBUG > 0 then puts "Count= #{$count}" end
          if is_possible(j,i,val) == true then
            $puzzle[j][i] = val
            if solve == 2 then return 2 end

            $puzzle[j][i] = 0
          end
        end
        return
      end
    end
  end
  print_puzzle
  puts "\nSolved in Iterations=#{$count}"
  puts
  return 2
end

$puzzle = Array.new(9) { Array.new(9) }
$DEBUG = 0
start = Time.now

##### Main Program Starts Here #####
# For each .matrix file supplied on the commandline run the solver
ARGV.each do |datafile|
  if datafile.end_with? '.matrix'
    # puts "Argument: #{datafile}"
    read_file(datafile)
    print_puzzle
    $count = 0
    solve
  end
end

puts "Seconds to process #{(Time.now - start).round(3)}"
