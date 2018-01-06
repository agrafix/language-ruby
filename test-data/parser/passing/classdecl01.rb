class Foo
  def method
    23
  end
end

class Bar < Foo
  def method
    42
  end

  def otherMethod(a, b)
    b - a
  end
end
