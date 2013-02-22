def foo():
  x = 13
  print(x)
  
  def bar():
    nonlocal x
    print(x)
    x = 42
    print(x)
  
  bar()
  print(x)

foo()
