a = lambda:print("Hello world")
b = lambda : lambda : lambda : print("hi")
c = lambda x, y, z : lambda a, b, c : a()
