{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Control.Applicative
import Control.Lens
import Data.Aeson.Lens()
import qualified Data.ByteString as BS
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

-- | The type of api function calls. Every call aside from those to "vim" take
--   a remote object as the first parameter.
data Function = Function
    { _canFail :: Bool
    , _id :: Int
    , _name :: Text
    , _parameters :: [Parameter]
    , _returnType :: Text
    } deriving (Eq, Show)

-- | The types of the parameters of functions
data Parameter = Parameter
    { _ptype :: Text -- ^ Parameter type.
    , _pname :: Text -- ^ Parameter name.
    } deriving (Eq, Show)

-- | The type of API metadata, consisting of classes and their operations.
data Api = Api
    { _classes :: [Class]
    , _functions :: [Function]
    } deriving (Eq, Show)

os :: Text -> Object
os = ObjectString

-- | Convert from a generic MessagePack object to type API metadata.
_Api :: Prism' Object Api
_Api = prism' focus refract
  where
    refract (ObjectMap m) = Api <$> classes <*> functions
      where
        f k pris = ix (os k) . _ObjectArray . below pris
        classes = m ^? f "classes" _Class
        functions = m ^? f "functions" _Function
    refract _ = Nothing

    focus (Api cs fs) = ObjectMap $ M.fromList
        [ (os "classes", classes)
        , (os "functions", functions)
        ]
      where
        f pris = review $ _ObjectArray . below pris
        classes = f _Class cs
        functions = f _Function fs

_Bool :: Prism' Text Bool
_Bool = prism' focus refract
  where
    focus True = "True"
    focus False = "False"
    refract "True" = pure True
    refract "False" = pure False
    refract _ = Nothing

_Class :: Prism' Object Class
_Class = _ObjectString . iso Class (\(Class n) -> n)

_Parameter :: Prism' Object Parameter
_Parameter = prism' focus refract
  where
    focus (Parameter name rtype) = ObjectArray [os name, os rtype]
    refract (ObjectArray x) = Parameter <$> rtype <*> name
      where
        rtype = x ^? ix 0 . _ObjectString
        name = x ^? ix 1 . _ObjectString
    refract _ = Nothing

_Function :: Prism' Object Function
_Function = prism' focus refract
  where
    refract (ObjectMap m) = Function
        <$> pure (fromMaybe False $ fail_ m)
        <*> (m ^? fid)
        <*> (m ^? name)
        <*> (m ^? ix (os "parameters") . parameters)
        <*> (m ^? returnType)
    refract _ = Nothing

    focus (Function c i n p r) = ObjectMap m
      where
        ps = review parameters p
        m = M.fromList
            [ (os "can_fail", review canFail c)
            , (os "id", ObjectInt i)
            , (os "name", os n)
            , (os "parameters", ps)
            , (os "return_type", os r)
            ]

    canFail :: Prism' Object Bool
    canFail = _ObjectString . _Bool
    fail_ = preview $ ix (os "can_fail") . canFail
    fid = ix (os "id") . _ObjectInt
    name = ix (os "name") . _ObjectString
    parameters :: Prism' Object [Parameter]
    parameters = _ObjectArray . below _Parameter
    returnType = ix (os "return_type") . _ObjectString
