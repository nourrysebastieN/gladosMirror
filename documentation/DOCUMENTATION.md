# Documentation

* [Syntax](#syntax)

### Syntax

> You can check the full Railroad page [here](https://agakitsune.github.io/dawn/dawn.html)

The syntax is very simple, and it's based on the Haskell syntax.

```haskell
data Maybe a = Just a | Nothing
```

```dawn
struct Maybe a {
    Just a
    Nothing
}
```

```haskell
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing"
```

```dawn
fromJust - Maybe a @ a
fromJust {
    (Just a) = a
    Nothing = error "Nothing"
}
```
