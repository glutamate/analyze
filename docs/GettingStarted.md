# Getting started with Analyze
# **NOTE: NOTHING OF THIS HAS BEEN IMPLEMENTED YET, THIS IS A DESIGN DOCUMENT**
_Taken from [Deedle in 10 minutes](http://bluemountaincapital.github.io/Deedle/tutorial.html), adapted to Analyze_

```haskell
import Analyze import Analyze.Frame as Frame import Analyze.Series as Series ``` 
## Creating series and frames

We can create a series with any type that instantiates
the `IsList` type class.

```haskell
dates = [ "A"
        , "A"
        , "A"
        ]

values = [10, 20, 30]

firstSeries = Series.new dates values
```

Also, we can create a series from a list of tuples:

```haskell
secondSeries = Series.ofObservations
    [ ( "A", 10)
    , ( "B", 20)
    , ( "C", 30)
    ]
```

We can create a series of implicit (ordinal) keys by doing:

```haskell
thirdSeries = Series.ofValues [ 10.0, 20.0, 30.0 ]
```

Now we can create a Frame using the first and the second Series,
as they share the keys.

```haskell
df1 = Frame.new ["first", "second"] [firstSeries, secondSeries]
```

A Frame has two type parameters: `Frame rowKey columnKey`. We use
this to index. The types of the data itself are not specified, and
instead, we do so when getting the data from the Frame.

We can create a data frame with `Int` indexes for rows or columns by:

```haskell
df2 = Frame.ofColumns [ ("first", firstSeries), ("second", secondSeries) ]
df3 = Frames.ofRows [ ("first", firstSeries), ("second", secondSeries)]
```

Also, we can specify our indexes for rows and columns by specifying
`(rowKey, columnKey, value)`:

```haskell
df4 = Frame.ofValues
    [ ("Monday", "John", 1.0)
    , ("Tuesday", "Joe", 2.1)
    , ("Tuesday", "John", 4.0)
    , ("Wednesday", "John", -5.4)
    ]
```

If your data types derive the `Generic` type class you can create a
data frame from a list of those too:

```haskell
data Price = Price
    { day :: Text
    , quantity :: Float
    } deriving (Generic)

instance Serialize Price

prices :: [Price]
prices =
    [ Price "1-1-17" 10.0
    , Price "2-1-17" 12.0
    , Price "3-1-17" 13.0
    ]

df5 = Frame.ofRecords prices
```

Finally, we can also load a data frame from CSV.

```haskell
msftCsv = Frame.readCsv "resources/MSFT.csv"
fbCsv = Frame.readCsv "resources/FB.csv"
```

The types are not inferred when loading like that, the user must specify them
later.


## Specifying index and joining

```haskell
msftOrd <- Frame.withFrame msftCsv $ do
    Frame.indexRowsDate "Date"
    Frame.sortRowsByKey
```

We can now get only the open and close prices, and add a new column.

```haskell
msft <- Frame.withFrame msftOrd $ do
    Frame.sliceColumns [ "Open", "Close" ]
    openColumn       <- Frame.getColumn "Open"
    closeColumn      <- Frame.getColumn "Close"
    let differenceColumn = zipWith (-) openColumn closeColumn
    Frame.addColumn "Difference" differenceColumn
```

We can do the same thing for Facebook:

```haskell
fb <- Frame.withFrame fbCsv $ do
    Frame.indexRowsDate "Date"
    Frame.sortRowsByKey
    Frame.sliceColumns [ "Open", "Close" ]
    openColumn       <- Frame.getColumn "Open"
    closeColumn      <- Frame.getColumn "Close"
    let differenceColumn = zipWith (-) openColumn closeColumn
    Frame.addColumn "Difference" differenceColumn
```

Let's create a single data frame that contains Microsoft and Facebook data.
Before joining those data frames, we have to rename their columns so their names
aren't duplicated.

```haskell
let msftNames = ["MsftOpen", "MsftClose", "MsftDiff"]
msftRenamed <- Frame.withFrame msft $
    Frame.indexColumnsWith msftNames

let fbNames = ["FbOpen", "FbClose", "FbDiff"]
fbRenamed <- Frame.withFrame fb $
    Frame.indexColumnsWith fbNames

let joinedOut = Frame.withFrame msftRenamed $
    Frame.outerJoin fbRenamed

let joinedIn = Frame.withFrame msftRenamed $
    Frame.innerJoin fbRenamed
```

## Selecting values and slicing

```haskell
let val  = Frame.getRow (Data.Time.Calendar.fromGregorian 2013 1 2) joinedIn
let val' = Frame.getRow (Data.Time.Calendar.fromGregorian 2013 1 2) joinedIn
         & Series.get "FbOpen"
```


## Projection and filtering

```haskell
-- 'modifying' modifies the original 'Frame' instead of copying it
Frame.modifying joinedOut $ do
    comparison <- Frame.mapRowValues $ \row ->
        if (Series.get "msftOpen" row) > (Series.get "fbOpen" row)
            then "MSFT"
            else "FB"
    Frame.addColumn "Comparison" comparison
```

We can now get the number of days when Microsoft stock prices were above Facebook
and the other way round:

```haskell
let msftCount = Frame.withFrame joinedOut $ do
    Frames.getColumn "Comparison" (Series.as :: String)
    >>= Series.filterValues (== "MSFT")
    >>= Series.countValues
-- msftCount = 220

let fbCount = Frame.withFrame joinedOut $ do
    Frames.getColumn "Comparison" (Series.as :: String)
    >>= Series.filterValues (== "FB")
    >>= Series.countValues
-- fbCount = 103
```


## Grouping and aggregation

Group rows by month and year:

```haskell
monthly <- Frames.withFrame joinedIn $
    Frame.groupRowsUsing $ \(y,m,_) _ -> fromGregorian y m 1
```

