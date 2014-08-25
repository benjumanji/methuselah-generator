{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Control.Applicative
import Control.Lens
import Data.Aeson.Lens()
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.MessagePack
import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Serialize


makePrisms ''Object


main :: IO ()
main = do x <- baz "data.bin"
          putStrLn . either id show $ x

baz :: String -> IO (Either String Object)
baz x = decode <$> BS.readFile x

data Class = Class Text deriving (Eq, Show)

data Function = Function
    { _canFail :: Bool
    , _id :: Int
    , _name :: Text
    , _parameters :: [Parameter]
    , _returnType :: Text
    } deriving (Eq, Show)

data Parameter = Parameter
    { _ptype :: Text
    , _pname :: Text
    } deriving (Eq, Show)

data Api = Api
    { _classes :: [Class]
    , _functions :: [Function]
    } deriving (Eq, Show)

toApi :: Object -> Maybe Api
toApi (ObjectMap m) = Just $ Api classes functions
  where
    classes = m ^.. ix (os "classes") . _ObjectArray . traverse . _Class
    functions = m ^.. ix (os "functions") . _ObjectArray . traverse . _Function
toApi _ = Nothing

fromObjectArray :: Text                -- ^ The key for the collection in the map
                -> (Object -> Maybe a) -- ^ An extraction function for a single item
                -> Map Object Object   -- ^ Top level map from meta data dump
                -> Maybe [a]
fromObjectArray key extract m = f =<< M.lookup okey m
  where
    okey = ObjectString key
    f (ObjectArray cs) = traverse extract cs
    f _ = Nothing

_Class :: Prism' Object Class
_Class = _ObjectString . f
  where
    f = iso Class (\(Class n) -> n)

_Function :: Prism' Object Function
_Function = prism' focus refract
  where
    refract (ObjectMap m) = Function <$> pure (fromMaybe True $ preview canFail m)
                                     <*> (m ^? fid)
                                     <*> (m ^? name)
                                     <*> pure (m ^.. parameters)
                                     <*> (m ^? returnType)
    refract _ = Nothing

    focus (Function c i n p r) = ObjectMap m
      where
        ps = ObjectArray $ map (review _Parameter) p
        m = M.fromList [ (os "can_fail", review (_ObjectString . _Bool) c)
                       , (os "id", ObjectInt i)
                       , (os "name", os n)
                       , (os "parameters", ps)
                       , (os "return_type", os r)
                       ]

    canFail = ix (os "can_fail") . _ObjectString . _Bool
    fid = ix (os "id") . _ObjectInt
    name = ix (os "name") . _ObjectString
    parameters = ix (os "parameters") . _ObjectArray . traverse . _Parameter
    returnType = ix (os "return_type") . _ObjectString

_Parameter :: Prism' Object Parameter
_Parameter = prism' focus refract
  where
    focus (Parameter name rtype) = ObjectArray [os name, os rtype]
    refract (ObjectArray x) = Parameter <$> rtype <*> name
      where
        rtype = x ^? ix 0 . _ObjectString
        name = x ^? ix 1 . _ObjectString
    refract _ = Nothing

os :: Text -> Object
os = ObjectString

_Bool :: Prism' Text Bool
_Bool = prism' focus refract
  where
    focus True = "True"
    focus False = "False"
    refract "True" = pure True
    refract "False" = pure False
    refract _ = Nothing
