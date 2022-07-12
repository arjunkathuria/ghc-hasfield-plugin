## GHC Source plugin to replace field matching with HasField accessor 

This plugin rewrite code like

```haskell
func :: Record -> Int
func Record{field1 = f1, field2 = f2} = f1 + f2
```

into

```haskell
func :: Record -> Int
func ((\x -> (GHC.Records.Compat.getField @"field1" x, GHC.Records.Compat.getField @"field2" x) -> (f1, f2)
  = f1 + f2
```

This code require ``HasField`` instances for ``Record` generated by RecordDotPreprocessor or written manually

## Copyrights

This work backed by Juspay Technologies Pvt Ltd and Monadfix OU