# Programming Languages, Homework 6, hw6runner.rb

# Jorge Fernando Flores Pinto
# V00880059
# CSC330 – Fall 2018

#require 'tk'

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here:
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # 5-long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]), # Square with extra block
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [1, 0], [0, 1]]), # small L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z

  Cheat_Piece = [[[0, 0]]]

  # Your Enhancements here
  def self.next_piece (board, with_cheat)
    if with_cheat
      Piece.new(Cheat_Piece, board)
    else
      Piece.new(All_My_Pieces.sample, board)
    end
  end

end

class MyBoard < Board
  # Your Enhancements here:
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self, @cheat)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    @current_block = MyPiece.next_piece(self, @cheat)
    @current_pos = nil
    if @cheat
      @score -= 100
    end
    @cheat = false
  end

  def cheat
    if @score > 100 and !game_over?
      @cheat = true;
    end
  end

  def store_current
    locations = @current_block.current_rotation
    num_rotations = @current_block.current_rotation.size
    displacement = @current_block.position
    (0..(num_rotations - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end

###################### Challenge ##############################
class MyTetrisChallenge < MyTetris
    attr_reader :panel

    def initialize
      @root = TkRoot.new('height' => 615, 'width' => 300,
               'background' => 'lightblue') {title "Tetris"}
      @timer = TetrisTimer.new
      set_next_piece_panel
      set_board
      @running = true
      key_bindings
      buttons
      run_game
    end

    def set_board
      @canvas = TetrisCanvas.new
      @board = MyBoardChallenge.new(self)
      @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
      @board.draw

    end

    def set_next_piece_panel
      label = TetrisLabel.new(@root) do
        text ' Next piece: '
        background 'LightBlue3'
      end

      @panel = TetrisPanel.new
      @panel.place(100, 100, 190, 250)
      label.place(30, 80, 200, 200)
    end

    def draw_piece_in_panel (piece, old=nil)
      if old != nil and piece.moved
        old.each{|block| block.remove}
      end
      size = 15
      blocks = piece.current_rotation
      start = [piece.current_rotation.size / 2 + 1,
           piece.current_rotation.size / 2 + 1]

      blocks.map{|block|
      MyTetrisRect.new(@panel, start[0]*size + block[0]*size + 3,
                         start[1]*size + block[1]*size,
                         start[0]*size + size + block[0]*size + 3,
                         start[1]*size + size + block[1]*size,
                         piece.color)}
    end
end

class MyPieceChallenge < MyPiece

end

class MyBoardChallenge < MyBoard
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self, @cheat)
    @npiece = MyPiece.next_piece(self, @cheat)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
    @next_piece_pos = @game.draw_piece_in_panel(@npiece, @next_piece_pos)

  end

  def draw
    @current_pos = @game.draw_piece(@current_block, @current_pos)

  end

  def next_piece
    @current_block = @npiece
    @npiece = MyPiece.next_piece(self, @cheat)
    @next_piece_pos = @game.draw_piece_in_panel(@npiece, @next_piece_pos)
    @current_pos = nil

  end

  def cheat
    if @score > 100 and !game_over?
      @cheat = true;
      @npiece = MyPiece.next_piece(self, @cheat)
      @next_piece_pos = @game.draw_piece_in_panel(@npiece, @next_piece_pos)
      @score -= 100
      @cheat = false
    end
  end

  attr_reader :npiece, :next_piece_pos

end

class MyTetrisRect
  def initialize(wrapped_panel, a, b, c, d, color)
    unwrapped_panel = wrapped_panel.panel
    @rect = TkcRectangle.new(unwrapped_panel, a, b, c, d,
                             'outline' => 'black', 'fill' => color)
  end

  def remove
    @rect.remove
  end

  def move(dx, dy)
    @rect.move(dx, dy)
  end

end

class TetrisPanel
  def initialize
    @panel = TkCanvas.new('background' => 'grey')
  end

  def place(height, width, x, y)
    @panel.place('height' => height, 'width' => width, 'x' => x, 'y' => y)
  end

  def unplace
    @panel.unplace
  end

  def delete
    @panel.delete
  end

  # Necessary so we can unwrap before passing to Tk in some instances.
  # Student code MUST NOT CALL THIS.
   attr_reader :panel
end
