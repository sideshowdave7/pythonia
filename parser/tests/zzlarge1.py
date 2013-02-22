global a

def useless_init(arg):
 pass

def moar_func():
 x = []
 for i in range(10):
  x.append(lambda x, y: i*-x+y)

def main():
 useless_init("a" "b")
 def iner_main(arg1, arg2):
  return arg1[arg2 >> (3 + 4)]
 ident = lambda x: x
 b = lambda: 3
 ident(iner_main([1, 2, 3, 4], 12))()
 x = moar_func()


if __name__ == '__main__'():
 main()

