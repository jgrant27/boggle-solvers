#!/usr/bin/env ruby

# File boggle.rb


#require 'enumerator'
require 'pp'
require 'solver'
require 'board_generator'


# If there's a command-line argument use it as the Boggle board.
# Otherwise create a random Boggle board.

board_count = 1
board_count = ARGV[0].to_i if ARGV.size >= 1


config = ""

unless STDIN.tty?  # we are in a pipeline
  while((line = STDIN.gets))
    config << line
  end
else
end

board = BoardGenerator.new(config).board_2d


#puts "\nLoading dictionary ...\n"
solver = BoggleSolver::Solver.new('../../dict/sowpods.txt')

puts "\nsolving %i boards ...\n" % board_count

t1 = Time.now

# Solve all the boards
#(1..board_count).each do |c|
  #BoardGenerator.seed = rand(2 ** 32)
  #random_board = BoardGenerator.new
  # Boggle board
  #board_string = random_board.to_input_s
  #puts "\nRandom board #%i is:" % c
  #puts random_board
  #board = random_board.board_2d

#  puts "\n"

  results = solver.solve(board)
  solver.solve(board)
  #puts "\nResults for board:"
  #puts results
#end

t2 = Time.now

puts "\nStats:"
puts "Boards solved: %i" % board_count
puts "Avg. time to solve per board: %0.6f seconds" % ((t2 - t1) / board_count)
puts "Total Time to solve all boards: %0.6f seconds" % (t2 - t1)

