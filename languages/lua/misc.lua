function foo(n)
  if n == 0 then
    return
  end
  print("foo")
  print(n)
  bar(n-1)
end

function bar(n)
  if n == 0 then
    return
  end
  print("bar")
  print(n)
  foo(n-1)
end

function breakLoop(max)
  local n = 0
  while true do
    print(n)
    n = n + 1
    if n == max then
      break
    end
  end
  print("used break")
end

function fact(n)
  local i = 1
  repeat
    i = i * n
    n = n - 1
  until n <= 0
  return i
end

for i=1,10 do
  print(fact(i))
end

foo(10)
bar(5)
breakLoop(7)

do
  print("in block")
  print((1 + 3) * 3 / 2)
end
