# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

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

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
  
  # Shift a geometry value by (x, y)
  def shift (dx, dy)
    self          # Default behavior, returns it self
  end
  def preprocess_prog
    self
  end
  def eval_prog env
    self
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

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
  
  def shift(dx, dy)
    Point.new(x + dx, y + dy)
  end
  
  def intersect other
    other.intersectPoint self
  end
  def intersectPoint p
    if real_close_point(x, y, p::x, p::y)
	  p
	else
	  NoPoints.new
	end
  end 
  def intersectLine line
    if real_close_point(x,y, x, line::m * x + line::b)
	  self
	else
	  NoPoints.new
	end
  end
  def intersectVerticalLine vline
    if real_close(x, vline::x)
	  self
	else
	  NoPoints.new
	end
  end
  def intersectWithSegmentAsLineResult seg
     if (real_close(@x, seg.x1) or (@x > seg.x1)) and (real_close(@x, seg.x2) or (@x < seg.x2))
	   self
	 else
	   NoPoints.new
	 end
  end
    
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end
  
  def shift(dx, dy)
    Line.new(m, b + dy - m*dx)
  end
  def intersect other
    other.intersectLine self
  end
  def intersectPoint p
    p.intersectLine self
  end
  def intersectLine line
    if real_close(m, line::m)
	  if real_close(b, line::b)
	    return line
      else
      	  return NoPoints.new
	  end
	else
	    inter_x = (b - line.b)/(line.m - m)
		inter_y = m*inter_x + b
		Point.new(inter_x, inter_y)
	end
  end
  def intersectVerticalLine vline
    Point(vline::x, m*vline::x + b)
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  def shift(dx, dy)
    VerticalLine.new(x + dx)
  end
  def intersect other
    other.intersectVerticalLine self
  end
  def intersectPoint p
    p.intersectVerticalLine self
  end
  def intersectLine line
    line.intersectVerticalLine self
  end
  def intersectVerticalLine vline
    if real_close(x, vline::x)
	  self
	else
	  NoPoints.new
	end
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end

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
  def preprocess_prog
    # No line segment has two real close end points, 
	# if that is the case, turn the line segment to a point.
	if real_close_point(x1, y1, x2, y2)
	  return Point.new(x1, y1)
	end
	
	# Order two end points such that the start point is either 
	# at the left or below the end point.
    tmp_x1, tmp_y1, tmp_x2, tmp_y2 = @x1, @y1, @x2, @y2
	if ((x1 > x2) and !real_close(x1, x2)) or ((y1 > y2) and !real_close(y1, y2))
	  tmp_x1, tmp_y1, tmp_x2, tmp_y2 = @x2, @y2, @x1, @y1
    end
	
	LineSegment.new(tmp_x1, tmp_y1, tmp_x2, tmp_y2)
  end
  
  def shift(dx, dy)
    LineSegment.new(x1+dx, y1+dy, x2+dx, y2+dy)
  end	
  
  def intersect other
    other.intersectLineSegment self
  end
  def intersectPoint p
    p.intersectLineSegment self
  end
  def intersectLine line
    line.intersectLineSegment self
  end
  def intersectVerticalLine vline
    vline.intersectLineSegment self
  end
  def intersectWithSegmentAsLineResult seg
    x1_start, y1_start, x1_end, y1_end = @x1, @y1, @x2, @y2
	x2_start, y2_start, x2_end, y2_end = seg.x1, seg.y1, seg.x2, seg.y2
	
	if real_close(x1_start, x1_end)
	  # The line segment is on a vertical line
	  # let segment a start at or below start of segment be
	  seg_a, seg_b = [[x1_start, y1_start, x1_end, y1_end], [x2_start, y2_start, x2_end, y2_end]]
      if y2_start > y1_start
	    seg_a, seg_b = [seg_b, seg_a]
	  end
	  ax_start, ay_start, ax_end, ay_end, bx_start, by_start, bx_end, by_end = seg_a + seg_b
	  if real_close(ay_end, by_start)
	    # Just touching
	    return Point.new(ax_end, ay_end)
	  elsif ay_end < by_start
	    # Disjoint
	    return NoPoints.new
	  elsif ay_end > by_end
	    # b inside a
		return LineSegment.new(bx_start, by_start, bx_end, by_end)
	  else 
	    # Overlapping
		return LineSegment.new(bx_start, by_start, ax_end, ay_end)
	  end
	else # the segments are not on a vertical line
	     # let a start at or to the left of start of segment be
	  seg_a, seg_b = [[x1_start, y1_start, x1_end, y1_end], [x2_start, y2_start, x2_end, y2_end]]
	  if x2_start < x1_start
	    seg_b, seg_a = [seg_a, seg_b]
      end
	  ax_start, ay_start, ax_end, ay_end, bx_start, by_start, bx_end, by_end = seg_a + seg_b
	  
	  if real_close(ax_end, bx_start)
	    # Just touching
		return Point.new(ax_end, ay_end)
	  elsif ax_end < bx_start
	    # Disjoint
		return NoPoints.new
	  elsif ax_end > bx_end
	    # b in a
		return LineSegment.new(bx_start, by_start, bx_end, by_end)
	  else
	    # Overlapping
		return LineSegment.new(bx_start, by_start, ax_end, ay_end)
	  end
	end
  end	    	  
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
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
  def eval_prog env
    (@e1.eval_prog env).intersect (@e2.eval_prog env)
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
  
  # Method to preprocess the expression
  def preprocess_prog
    p_e1 = @e1.preprocess_prog
	p_e2 = @e2.preprocess_prog
	Let.new(@s, p_e1, p_e2)
  end
  def eval_prog env
    new_env = [[@s, (@e1.eval_prog env)]] + env
	@e2.eval_prog new_env
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
  
  # Trivil case of preprocessing
  def preprocess_prog
    self
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
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
  
  def eval_prog env
    e_res = (@e.eval_prog env)
	e_res.shift(@dx, @dy)
  end
end
