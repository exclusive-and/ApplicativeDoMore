
# Applicative Do More

`ApplicativeDoMore` is a GHC plugin that rewrites do-expressions so that
let-statements don't incur a `Monad` constraint when using `ApplicativeDo`.

The plugin facilitates simulating and designing hardware in Haskell and Clash.
It gives programmers a way to do combinational logic directly on `Signal`s,
similar to LLHD's `prb` and `drv` instructions. Gone are the days of `fmap`ing
everything by hand.


## Example

The following code sample incurs a `Monad` constraint when compiled by vanilla
GHC with `ApplicativeDo` enabled:

```haskell
blinky :: HiddenClockResetEnable dom => Signal dom Bool
blinky = blink
    where
    blink = register False blink'
    count = register (0 :: Unsigned 8) count'

    (blink', count') = unbundle $ do
        b <- blink
        c <- count

        let b' = if c `mod` 5 == 4 then not b else b
            c' = c + 1

        pure (b', c')
```

The sample incurs the `Monad` constraint because GHC's
`rearrangeForApplicativeDo` function sees the do-expression as:

```haskell
HsDo [ ApplicativeStmt [ b <- blink, c <- count ]
     , LetStmt [ b' = if c `mod` 5 == 4 then not b else b
               , c' = c + 1 ]
     , LastStmt (pure (b', c'))
     ]
```

The plugin will take that AST created by `rearrangeForApplicativeDo`, and
rewrite it so that let-statements are inlined as far as possible to get:

```haskell
HsDo [ ApplicativeStmt [ b <- blink, c <- count ]
     , LastStmt ( HsLet [ b' = if c `mod` 5 == 4 then not b else b
                        , c' = c + 1 ] in (b', c') )
     ]
```

After rewriting so that let bindings are inlined into the last statement and
`join`s are removed, the typechecker infers that this do-expression only
incurs an `Applicative` constraint.
