To test decoding and encoding in GHCi:

```haskell
traverse print $ map decodePersons $ replicate 3 testPersonData
```

```haskell
persons = decodePersons testPersonData
(== testPersonData) $ encodePersons $ Data.Either.fromRight undefined $ decodePersons testPersonData
```