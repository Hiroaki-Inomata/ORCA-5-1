#!/usr/bin/ruby1.8

packages = ["bar-1.0", "foo-1.0", "foo-1.1", "foo-1.2", "baz-1.0"]

puts `rm -f *jpp *.asc`

packages.each{|p|
  puts `tar zcvf #{p}.jpp #{p}`
  puts `gpg -abs #{p}.jpp`
}

lists = ["list1.yml","list2.yml","list3.yml","list4.yml"]
lists.each{|l|
  puts `gpg -abs #{l}`
}
