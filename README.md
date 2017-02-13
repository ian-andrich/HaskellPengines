Haskell Pengines v.0.0.2
# This is the basic Pengines interface for Haskell.
It aims to provide a simple API for Haskell integration with SWI-Prolog's Pengines.

## To Do:
We are currently working on...
- Basic implementation of interface. TODO
- Get a prototype working. COMPLETED
- Model the datatypes.  Ensure relevant datatypes are serializable.
- Necessary datatypes include Prolog Data Types, Header Data (Sent), Header Data (Received), Query Data, Server Data, along with default values for each. 
- Add tests, and error handling.
- Finish re-write of prototype code.

## Split Projects:
Need to split off the Prolog language stuff into a separate library.

## Desired Interface
Basic configuration assuming a pengine server running locally on port 4242.
Throughout, we use the query for `member(X, [a,b,c]).` as our basic Pengines Query.
For more information on Prolog Syntax, please go to [SWIPL](http://www.swi-prolog.org/)
```
pengineServer = "http://localhost:4242"

query = "member(X, [a,b,c])."
```
1: A quick one off query of the server.  The connection is automatically closed afterwards.
```
userQuery :: String -> String -> IO (Hashmap.map String String)
userQuery pengineServer query = queryPengineOnce $ pengineServer query
```

2: A lazy list of query results.  The connection is automatically closed once it falls out of scope.

```
lazyUserQuery pengineServer query = queryPengineLazy $ pengineServer query
```

3: Manually perform queries.

```
userManualQuery = do
    pengineHandle <- pengineConnect server
    pengineQuery1 <- pengineQuery pengineHandle query
    pengineQuery2 <- pengineNext pengineHandle
    pengineClose pengineHandle
```
