# File trie.rb



# Implements the trie data structure.
class Trie
  def initialize(level = 0)
    @level = level
    @hash = Hash.new
  end


  # Adds the string parameter to the trie.
  def add(string)
    letter = curr_letter(string)
    if letter.nil?
      @hash[letter] = string
    else
      trie = @hash[letter]
      if trie.nil?
        trie = @hash[letter] = Trie.new(@level + 1)
      end
      trie.add(string)
    end
  end


  # Tests whether the string parameter is a whole word present within
  # the trie.
  def include?(string)
    letter = curr_letter(string)
    if letter.nil?
      @hash[nil] == string
    else
      trie = @hash[letter]
      trie && trie.include?(string)
    end
  end


  # Tests whether there are any words in the trie that *begin* with
  # the string parameter.
  def begin?(string)
    letter = curr_letter(string)
    if letter.nil?
      @hash
    else
      trie = @hash[letter]
      trie && trie.begin?(string)
    end
  end


  # From the current trie, returns the sub-trie where the string
  # parameter is the next letter.
  def subtrie(letter)
    trie = @hash[letter]
    if trie.nil?
      trie = @hash[letter] = Trie.new(@level + 1)
    end
    trie
  end


  # Returns true if there are any words in the current trie.
  def any?
    @hash.size > 0
  end


  # Loads in the text file passed in by filename, treating each line
  # as a word to load into the trie.
  def self.from_dictionary(filename)
    trie = Trie.new
    IO::foreach(filename) { |line| line.chomp!.upcase! ; trie.add(line) }
    trie
  end


  protected


  # Returns the letter of the string passed in corresponding ot the
  # level of this trie (or sub-trie).
  def curr_letter(string)
    letter = string[@level, 1]
    letter && letter.empty? ? nil : letter
  end
end

