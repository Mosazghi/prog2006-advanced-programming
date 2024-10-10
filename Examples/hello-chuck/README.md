# hello-chuck implementation in Haskell

Same as in the Golang for PROG2005, we fetch a joke from
a Chuck jokes web API, parse the JSON, and print the joke.
[Check the Go version](https://github.com/gtl-hig/chuckjoke/)

This version of the code handles errors and prints the error
code if the joke could not be fetched.



# Generic JSON derivation and duplicate fields

Note, json records often have the same field names, eg. "id".
This becomes a problem in Haskell because there should only be
one function declaration with a given name, and record data
types declares accessor functions for us.

* One can use [a language extension, DuplicateRecordFields](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html) that allows record field duplication
* Another [longer discussion on this has been provided in SO](https://stackoverflow.com/questions/35702440/handling-id-in-derived-aeson-fromjson-instances-with-aeson-json)


In general however, it is often necessary to build
your own JSON parser and handle fields, and types dynamically.
[This article goes over many nuances and techniques](https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html)
for json parsing in Haskell with the use of AESON library,
and without using simple Generic deriving.

