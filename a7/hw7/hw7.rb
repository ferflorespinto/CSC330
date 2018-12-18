# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2)
      (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2)
      real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2)
      if real_close(x1,x2)
        VerticalLine.new x1
      else
        m = (y2 - y1).to_f / (x2 - x1)
        b = y1 - m * x1
        Line.new(m,b)
      end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  def eval_prog(env)
    self
  end
  def preprocess_prog
    self
  end
  def shift(dx,dy)
    Point.new(x + dx, y + dy)
  end
  def intersect(val)
    val.intersectPoint self
  end
  def intersectPoint(pt)
    if (real_close(@x, pt.x) and real_close(@y, pt.y))
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end
  def intersectLine(l)
    y = l.m * @x + l.b
    if (real_close(y, @y))
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end
  def intersectVerticalLine(vl)
    if (real_close(vl.x, @x))
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end
  def intersectWithSegmentAsLineResult seg
    if (does_point_lie_between_segment(@x, @y, seg))
      Point.new(@x, @y)
    else
      NoPoints.new
    end
  end
  def does_point_lie_between_segment(x_0, y_0, seg)
    ((real_close(x_0, seg.x1) or x_0 > seg.x1) and
    (real_close(x_0, seg.x2) or x_0 < seg.x2) and
    (real_close(y_0, seg.y1) or y_0 > seg.y1) and
    (real_close(y_0, seg.y2) or y_0 < seg.y2))
  end
  #def to_s
  #  str = "Point(x=" + @x.to_s + ", y=" + @y.to_s + ")"
  #  str
  #end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b
  def initialize(m,b)
    @m = m
    @b = b
  end
  def eval_prog(env)
    self
  end
  def preprocess_prog
    self
  end
  def shift(dx,dy)
    Line.new(@m, @b + dy - @m * dx)
  end
  def intersect(val)
    val.intersectLine self
  end
  def intersectPoint(pt)
    y = @m * pt.x + @b
    if (real_close(y, pt.y))
      Point.new(pt.x, pt.y)
    else
      NoPoints.new
    end
  end
  def intersectLine(l)
    if (real_close(@m, l.m))
      if (real_close(@b, l.b))
        Line.new(@m, @b)
      else
        NoPoints.new
      end
    else
      x = (l.b - @b) / (@m - l.m)
      y = @m * x + @b
      Point.new(x, y)
    end
  end
  def intersectVerticalLine(vl)
    y = @m * vl.x + @b
    Point.new(vl.x, y)
  end

  #def to_s
  #  str =  "Line(m = " + @m.to_s + ", b = " + @b.to_s + ")"
  #  str
  #end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  def eval_prog(env)
    self
  end
  def preprocess_prog
    self
  end
  def shift(dx, dy)
    VerticalLine.new(@x + dx)
  end
  def intersect(val)
    val.intersectVerticalLine self
  end
  def intersectPoint(pt)
    if (real_close(@x, pt.x))
      Point.new(pt.x, pt.y)
    else
      NoPoints.new
    end
  end
  def intersectLine(l)
    y = l.m * @x + l.b
    Point.new(@x, y)
  end
  def intersectVerticalLine(vl)
    if (real_close(@x, vl.x))
      VerticalLine.new(@x)
    else
      NoPoints.new
    end
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
  #def to_s
  #  str = "VerticalLine(x = " + @x.to_s + ")"
  #  str
  #end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end
  def eval_prog(env)
    self
  end
  def preprocess_prog
    if (real_close(@x1, @x2))
      if (real_close(@y1, @y2))
        Point.new(@x1, @y1)
      else
        if (@y1 > @y2)
          LineSegment.new(@x2, @y2, @x1, @y1)
        else
          LineSegment.new(@x1, @y1, @x2, @y2)
        end
      end
    else
      if (@x1 > @x2)
        LineSegment.new(@x2, @y2, @x1, @y1)
      else
        LineSegment.new(@x1, @y1, @x2, @y2)
      end
    end
  end
  def shift(dx, dy)
    LineSegment.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 + dy)
  end
  #intersect is unfinished
  def intersect(val)
    val.intersectLineSegment self
  end
  def intersectPoint(pt)
    m = (@y2 - @y1) / (@x2 - @x1)
    y = m * (pt.x - @x1) + @y1

    if (real_close(y, pt.y) and y < @y2)
      Point.new(pt.x, pt.y)
    else
      NoPoints.new
    end
  end
  def intersectLine(l)
    m = (@y2 - @y1) / (@x2 - @x1)
    b = @y1 - (m * @x1)
    if (real_close(m, l.m))
      if (real_close(b, l.b))
        LineSegment.new(@x1, @y1, @x2, @y2)
      else
        NoPoints.new
      end
    else
      x_0 = (l.b - b) / (m - l.m)
      y_0 = m * x_0 + b
      #puts "(x_0:" + x_0.to_s + ", y_0:" + y_0.to_s + ")"
      if ((real_close(x_0, @x1) or x_0 > @x1) and
        (real_close(x_0, @x2) or x_0 < @x2) and
        (real_close(y_0, @y1) or y_0 > @y1) and
        (real_close(y_0, @y2) or y_0 < @y2))
        Point.new(x_0, y_0)
      else
        NoPoints.new
      end
    end
  end
  def intersectVerticalLine(vl)
    m = (@y2 - @y1) / (@x2 - @x1)
    b = @y1 - (m * @x1)
    y = m * vl.x + b
    if (@x1 == vl.x and @x2 == vl.x)
      LineSegment.new(@x1, @y1, @x2, @y2)
    elsif ((real_close(@x1, vl.x) or vl.x > @x1) and
      (real_close(@x2, vl.x) or vl.x < @x2))
      Point.new(vl.x, y)
    else
      NoPoints.new
    end
  end
  def intersectLineSegment(ls)
    second_m = (@y2 - @y1) / (@x2 - @x1)
    second_b = @y1 - (second_m * @x1)
    first_m = (ls.y2 - ls.y1) / (ls.x2 - ls.x1)
    first_b = ls.y1 - (first_m * ls.x1)

    highest_x1 = if (@x1 > ls.x1) then @x1 else ls.x1 end
    highest_y1 = if (@y1 > ls.y1) then @y1 else ls.y1 end
    lowest_x2 = if (@x2 < ls.x2) then @x2 else ls.x2 end
    lowest_y2 = if (@y2 < ls.y2) then @y2 else ls.y2 end

    if ((first_m.infinite? == 1 or first_m.infinite? == -1))
      if (second_m.infinite? == 1 or second_m.infinite? == -1)
        #puts "Both segments are vertical"
        if (@x1 == ls.x1)
          #puts "Both segments lie on the same x"
          if (real_close(highest_y1, lowest_y2))
            #puts "Segments intersect at endpoints"
            Point.new(@x1, highest_y1)
          elsif (highest_y1 < lowest_y2)
            #puts "Intersection is a segment"
            LineSegment.new(@x1, highest_y1, @x1, lowest_y2)
          else
            NoPoints.new
          end
        else
          NoPoints.new
        end
      else
        #puts "First line segment is not vertical, second line segment is vertical"
        x_0 = ls.x1
        y_0 = second_m * x_0 + second_b
        #puts "(x_0:" + x_0.to_s + ", y_0:" + y_0.to_s + ")"

        if (does_point_lie_between_segment(x_0, y_0, self) == true)
          Point.new(x_0, y_0)
        else
          NoPoints.new
        end
      end
    elsif (real_close(@x1, ls.x1) and real_close(@y1, ls.y1) and real_close(@x2, ls.x2) and real_close(@y2, ls.y2))
      LineSegment.new(@x1, @y1, @x2, @y2)
    else
      if (real_close(second_m, first_m))
        if (real_close(second_b, first_b))
          #puts "Both segments represent the same line"
          if (real_close(lowest_x2, highest_x1) and (real_close(lowest_y2, highest_y1)))
            #puts "Segments intersect at ends"
            Point.new(highest_x1, highest_y1)
          elsif (lowest_x2 > highest_x1 and lowest_y2 > highest_y1)
            LineSegment.new(highest_x1, highest_y1, lowest_x2, lowest_y2)
          else
            NoPoints.new
          end
        else
          #puts "Same slope, but different intercepts"
          NoPoints.new
        end
      else
        x_0 = (first_b - second_b) / (second_m - first_m)
        y_0 = first_m * x_0 + first_b
        #puts "(x_0:" + x_0.to_s + ", y_0:" + y_0.to_s + ")"
        if (does_point_lie_between_segment(x_0, y_0, self) == true)
          Point.new(x_0, y_0)
        else
          NoPoints.new
        end
      end
    end
  end

  def does_point_lie_between_segment(x_0, y_0, seg)
    ((real_close(x_0, seg.x1) or x_0 > seg.x1) and
    (real_close(x_0, seg.x2) or x_0 < seg.x2) and
    (real_close(y_0, seg.y1) or y_0 > seg.y1) and
    (real_close(y_0, seg.y2) or y_0 < seg.y2))
  end
  #def to_s
  #  str = "LineSegment(" + @x1.to_s + ", " + @y1.to_s + ", " + @x2.to_s + ", " + @y2.to_s + ")"
  #  str
  #end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  def preprocess_prog
    @e1 = @e1.preprocess_prog
    @e2 = @e2.preprocess_prog
    self
  end
  def eval_prog(env)
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
  def preprocess_prog
    @e1 = @e1.preprocess_prog
    @e2 = @e2.preprocess_prog
    self
  end
  def eval_prog(env)
    list = [@s, @e1]
    newenv = env.unshift list
    pr = newenv.assoc @s
    puts pr.to_s
    raise "undefined variable" if pr.nil?
    @e2.eval_prog newenv
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def preprocess_prog
    self
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    puts pr.to_s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
  def preprocess_prog
    @e = @e.preprocess_prog
    self
  end
  def eval_prog(env)
    pt = @e.eval_prog(env)
    pt.shift(@dx, @dy)
  end
end
