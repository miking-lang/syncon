function testlua -a file
  if not stack build
    return
  end
  diff -y (cat lua/real-prelude $file | lua - | psub) (cat lua/fake-prelude $file | stack exec implementation-exe File core lua/language - | psub)
end

function timelua -a file
  if not stack build
    return
  end
  cat lua/real-prelude $file | time lua - > /dev/null
  cat lua/fake-prelude $file | time stack exec implementation-exe File core lua/language - > /dev/null
end

function testocaml -a file
  if not stack build
    return
  end
  diff -y (cat ocaml/real-prelude $file | ocaml -stdin | psub) (cat ocaml/fake-prelude $file | stack exec implementation-exe -- -iec File core ocaml/language - | psub)
end