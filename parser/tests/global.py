globalvar = 0

def foo():
  global globalvar
  globalvar = 10

foo()

print(globalvar)
