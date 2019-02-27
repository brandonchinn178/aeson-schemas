# aeson-schemas

A library that extracts information from JSON input using type-level schemas
and quasiquoters, consuming JSON data in a type-safe manner. Better than
`aeson` for decoding nested JSON data that would be cumbersome to represent as
Haskell ADTs.

## Quickstart

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import qualified Data.Text as T

-- First, define the schema of the JSON data
type MySchema = [schema|
  {
    users: List {
      id: Int,
      name: Text,
      age: Maybe Int,
      enabled: Bool,
      groups: Maybe List {
        id: Int,
        name: Text,
      },
    },
  }
|]

main :: IO ()
main = do
  -- Then, load data from a file
  obj <- either fail return =<<
    eitherDecodeFileStrict "examples/input.json" :: IO (Object MySchema)

  -- print all the users' ids
  print [get| obj.users[].id |]

  flip mapM_ [get| obj.users |] $ \user -> do
    -- for each user, print out some information
    putStrLn $ "Details for user #" ++ show [get| user.id |] ++ ":"
    putStrLn $ "* Name: " ++ T.unpack [get| user.name |]
    putStrLn $ "* Age: " ++ maybe "N/A" show [get| user.age |]
    case [get| user.groups |] of
      Nothing -> putStrLn "* No groups"
      Just groups -> putStrLn $ "* Groups: " ++ show groups
```

## Features

### Type safe

Since schemas are defined at the type level, parsing JSON objects is checked at
compile-time:

```
-- using schema from above
>>> [get| obj.users[].isEnabled |]

<interactive>:1:6: error:
    • Key 'isEnabled' does not exist in the following schema:
      '[ '("id", 'Data.Aeson.Schema.SchemaInt),
         '("name", 'Data.Aeson.Schema.SchemaText),
         '("age",
           'Data.Aeson.Schema.SchemaMaybe 'Data.Aeson.Schema.SchemaInt),
         '("enabled", 'Data.Aeson.Schema.SchemaBool),
         '("groups",
           'Data.Aeson.Schema.SchemaMaybe
             ('Data.Aeson.Schema.SchemaList
                ('Data.Aeson.Schema.SchemaObject
                   '[ '("id", 'Data.Aeson.Schema.SchemaInt),
                      '("name", 'Data.Aeson.Schema.SchemaText)])))]
    • In the second argument of ‘(.)’, namely ‘getKey @"isEnabled"’
      In the first argument of ‘(<$:>)’, namely
        ‘(id . getKey @"isEnabled")’
      In the first argument of ‘(.)’, namely
        ‘((id . getKey @"isEnabled") <$:>)’
```

### Point-free definitions

You can also use the `get` quasiquoter to define a pointfree function:

```haskell
getNames :: Object MySchema -> [Text]
getNames = [get| .users[].name |]
```

You can use the `unwrap` quasiquoter to define intermediate schemas:

```haskell
type User = [unwrap| MySchema.users[] |]

getUsers :: Object MySchema -> [User]
getUsers = [get| .users[] |]

groupNames :: User -> Maybe [Text]
groupNames = [get| .groups?[].name |]
```

## Advantages over `aeson`

### JSON keys that are invalid Haskell field names

`aeson` does a really good job of encoding and decoding JSON data into Haskell
values. Most of the time, however, you don't deal with encoding/decoding data
types manually, you would derive `Generic` and automatically derive `FromJSON`.
In this case, you would match the constructor field names with the keys in the
JSON data. The problem is that sometimes, JSON data just isn't suited for being
defined as Haskell ADTs. For example, take the following JSON data:

```json
{
    "id": 1,
    "type": "admin",
    "DOB": "5/23/90"
}
```

The `FromJSON` instance for this data is not able to be automatically generated
from `Generic` because the keys are not valid/ideal field names in Haskell:

```haskell
data Result = Result
  { id :: Int
    -- ^ `id` shadows `Prelude.id`
  , type :: String
    -- ^ `type` is a reserved keyword
  , DOB :: String
    -- ^ fields can't start with an uppercase letter
  } deriving (Generic, FromJSON)
```

The only option is to manually define `FromJSON` -- not a bad option, but less
than ideal.

With this library, you don't have these limitations:

```haskell
type Result = [schema|
  {
    id: Int,
    type: Text,
    DOB: Text,
  }
|]
```

### Nested data

What about nested data? If we wanted to represent nested JSON data as Haskell
data types, you would need to define a Haskell data type for each level.

```json
{
    "permissions": [
        {
            "resource": {
                "name": "secretdata.txt",
                "owner": {
                    "username": "john@example.com"
                }
            },
            "access": "READ"
        }
    ]
}
```

```haskell
data Result = Result
  { permissions :: [Permission]
  }
  deriving (Generic, FromJSON)

data Permission = Permission
  { resource :: Resource
  , access :: String
  } deriving (Generic, FromJSON)

data Resource = Resource
  { name :: String
  , owner :: Owner
  } deriving (Generic, FromJSON)

data Owner = Owner
  { username :: String
  }
```

It might be fine for a single example like this, but if you have to parse this
kind of data often, it'll quickly become cumbersome defining multiple data
types for each JSON schema. Additionally, the namespace becomes more polluted
with each data type. For example, if you imported all four of these data types,
you wouldn't be able to use `name`, `username`, `resource`, etc. as variable
names, which can become a pain.

Compared with this library:

```haskell
type Result = [schema|
  {
    permissions: List {
      resource: {
        name: Text,
        owner: {
          username: Text,
        },
      },
      access: Text,
    }
  }
|]
```

The only identifier added to the namespace is `Result`, and parsing out data
is easier and more readable:

```haskell
-- without aeson-schemas
map (username . owner . resource) . permissions

-- with aeson-schemas
[get| result.permissions[].resource.owner.username |]
```

### Duplicate JSON keys

Maybe you have nested data with JSON keys reused:

```json
{
    "_type": "user",
    "node": {
        "name": "John",
        "groups": [
            {
                "_type": "group",
                "node": {
                    "name": "Admin",
                    "writeAccess": true
                }
            }
        ]
    }
}
```

This might be represented as:

```haskell
data UserNode = UserNode
  { _type :: String
  , node :: User
  }

data User = User
  { name :: String
  , groups :: [GroupNode]
  }

data GroupNode = GroupNode
  { _type :: String
  , node :: Group
  }

data Group = Group
  { name :: String
  , writeAccess :: Bool
  }
```

Here, `_type`, `name`, and `node` are repeated. This works with
`{-# LANGUAGE DuplicateRecordFields #-}`, but you wouldn't be able to use the
accessor function anymore:

```
>>> node userNode

<interactive>:1:1: error:
    Ambiguous occurrence 'node'
    It could refer to either the field 'node',
                             defined at MyModule.hs:3:5
                          or the field 'node', defined at MyModule.hs:13:5
```

So you'd have to pattern match out the data you want:

```haskell
let UserNode{node = User{groups = userGroups}} = userNode
    groupNames = map (\GroupNode{node = Group{name = name}} -> name) userGroups
```

With this library, parsing is much more straightforward

```haskell
let groupNames = [get| userNode.node.groups[].node.name |]
```
