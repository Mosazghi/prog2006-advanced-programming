# Example of a Doctest and a Quick test set-up in a Stack project

To be able to use quick tests and doctests in a stack project, three packages need to be imported in the
```package.yaml``` file. It is ```doctest```, ```hspec``` and ```QuickCheck```.

The packages are inserted at the complete end of the file, in the dependencies of the tests section:

```yaml

tests:
  DoctestQuicktestExample-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - DoctestQuicktestExample
    - doctest
    - hspec
    - QuickCheck

```
After that, the tests can be run with the command ```stack test```.

For the rest, see the code in app/Main.hs, src/Lib.hs and test/Spec.hs. 

