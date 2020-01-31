# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
			   rotations([[0, 0], [-1, 0], [0, -1], [-1, -1], [1, 0]]), # Square with tail
			   rotations([[0, 0], [1, 0], [0, 1]]), # Square with one corner lost
			   [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], 
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]]] # Longer
			   
  Cheat_Piece = [[[0, 0]]] # Piece for cheat
  # your enhancements here
  
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  
  
  # Generate a cheat piece for the board
  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end

end

class MyBoard < Board
  # your enhancements here
  #Replace MyPiece for Piece
  def initialize(game)
    super
	@current_block = MyPiece.next_piece(self)
	@cheat = false   # If true, next piece will be a cheat piece.
  end
  
  
  # Rotates the current block 180 degree
  def rotate_180
    if !game_over? and @game.is_running?
	    @current_block.move(0, 0, 2)
    end
	draw
  end
  
  
  # Override for cheating
  def next_piece
    if @cheat
	  @current_block = MyPiece.cheat_piece(self)
	else
      @current_block = MyPiece.next_piece(self)
	end	
	@cheat = false
    @current_pos = nil
  end
  
  
  
  # Switch to cheat
  def cheat
    if (@score >= 100) and !@cheat
	  @score -= 100
	  @cheat = true
	end
    
  end	
  
  
  
  # Override to store blocks that has number of
  # peices different with 4
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
	num_pieces = locations.size
    (0..(num_pieces - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
end

class MyTetris < Tetris
  # your enhancements here
  def key_bindings
    super
	@root.bind('u', proc {@board.rotate_180})
	@root.bind('c', proc {@board.cheat})
  end
  
  
  #Set board to be MyBoard
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw  
  end

end


