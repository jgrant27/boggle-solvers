#!/usr/bin/env ruby

# Represents and generates Boggle Boards.  It randomly rolls simulated
# Boggle Dice and places them randomly in a 4x4 grid.
# Note: one die has the side 'QU'.



class BoardGenerator

  BoggleDie = [
               ['F', 'O', 'R', 'I', 'X', 'B'],
               ['M', 'O', 'QU', 'A', 'B', 'J'],
               ['G', 'U', 'R', 'I', 'L', 'W'],
               ['S', 'E', 'T', 'U', 'P', 'L'],
               ['C', 'M', 'P', 'D', 'A', 'E'],
               ['A', 'C', 'I', 'T', 'A', 'O'],
               ['S', 'L', 'C', 'R', 'A', 'E'],
               ['R', 'O', 'M', 'A', 'S', 'H'],
               ['N', 'O', 'D', 'E', 'S', 'W'],
               ['H', 'E', 'F', 'I', 'Y', 'E'],
               ['O', 'N', 'U', 'D', 'T', 'K'],
               ['T', 'E', 'V', 'I', 'G', 'N'],
               ['A', 'N', 'E', 'D', 'V', 'Z'],
               ['P', 'I', 'N', 'E', 'S', 'H'],
               ['A', 'B', 'I', 'L', 'Y', 'T'],
               ['G', 'K', 'Y', 'L', 'E', 'U']
              ]

  @@random_seed = 10202007

  attr_reader :board;
  attr_reader :dimensions;


  def initialize(config)

    rcnt = 0
    ccnt = 0
    @board = []
    config.strip.split("\n").each do |line|
      rcnt += 1
      ccnt = 0
      #line.strip.split.each { |c| @board << c ; ccnt += 1 } # for spaces
      line.each_byte { |b| @board << b.chr ; ccnt += 1 } # no spaces
    end

    @dimensions = [rcnt, ccnt]

    #@dimensions = [4 , 4]

    #@boggledie_cust = []

    #1.step(@dimensions[0] * @dimensions[0], BoggleDie.length) do |n|
    #  @boggledie_cust = @boggledie_cust + BoggleDie
    #end

    #puts @boggledie_cust.size

    #srand @@random_seed         # repeatable
    #indices = (0...@boggledie_cust.size).to_a.sort_by { rand }
    #@@random_seed = rand 2**31  # pick next random seed based on previous

    # creates an array with the dice randomly re-ordered and then
    # rolls each of the dice
    #@board = @boggledie_cust.values_at(*indices).map { |die| die[rand(die.size)] }

    #pp @board ; exit
  end


  def self.seed=(new_seed)
    @@random_seed = new_seed
  end


  # Returns an array of arrays, where outer arrays contains the rows
  # on the board, and each row is an array that contains the letters
  # on a given row.
  def board_2d
    # slice the array into groups of 4 to create 2d-array
    if RUBY_VERSION =~ /.*1\.9.*/
      enum = Enumerable::Enumerator.new(@board, :each)
      enum.each_slice(@dimensions[0]).to_a
    else
      require 'enumerator'
      @board.enum_slice(@dimensions[0]).to_a
    end
  end


  def to_input_s
    board.join('').gsub('QU', 'Q')
  end


  # Returns a string that displays the rows on separate lines.
  def to_s
    board_2d.map do |row|
      row.map { |letter| '%-3s' % letter }.join(' ')
    end.join("\n")
  end

end


if $0 == __FILE__
  BoardGenerator.seed = 555

  5.times do
    b = BoardGenerator.new
    p b.board      # 1-dimensional array
    p b.board_2d   # 2-dimensional array
    puts b         # 4-line string
    puts b.to_input_s
  end

end

