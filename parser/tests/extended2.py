def a():
 for i in range(10):
  yield i, i*i

for i in a():
 print(i)

