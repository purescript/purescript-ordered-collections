## Module Data.StrMap.Unsafe

#### `unsafeIndex`

``` purescript
unsafeIndex :: forall a. StrMap a -> String -> a
```

Unsafely get the value for a key in a map.

This function does not check whether the key exists in the map.


