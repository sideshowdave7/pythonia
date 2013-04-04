g = 200 

def f():
  if False:
    global g
  for g in [1,2,3]: pass
  x = 314
  def h():
    g = x
    print(g)
  h()
  print(g)
  return g 

print (f()) 
print (g)
