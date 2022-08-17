## Testing

The [HSpec](http://hspec.github.io/) framework is used for testing with the convention that each source file in the directory `/src` has its own test case described in the directory `/test`. See an example [here](http://hspec.github.io/hspec-discover.html).

## How to add tests

1) Add a file to the directory `test/Mllib/` with name *sourceFileName*Spec.hs (if the file already exists, write more tests in the same way and run them, skip next steps).

e.g.:
see `src/Mllib/Classification/SVM.hs` -> add `test/Mllib/Classification/SVMSpec.hs`

2) Define module and write tests (Look at the existing spec files in the `/test` and write in the same way)

3) Import the module in the `/test/Spec.hs` and describe the test case
```haskell
...
import qualified Mllib.Classification.SVMSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ...
  describe "Mllib.Classification.SVMSpec" Mllib.Classification.SVMSpec.spec
```

## How to run tests

In the root directory run `stack test`
