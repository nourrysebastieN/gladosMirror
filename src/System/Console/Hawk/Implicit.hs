{-# LANGUAGE RecordWildCards, PatternGuards #-}
{-|
    This module provides simple command line argument processing.
    The main function of interest is 'hawk'.
    A simple example is:

    @data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)@

@
sample = Sample{hello = 'def' '&=' 'help' \"World argument\" '&=' 'opt' \"world\"}
         '&=' 'summary' \"Sample v1\"
@

    @main = print =<< 'hawk' sample@

    Attributes are used to control a number of behaviours:

    * The help message: 'help', 'typ', 'details', 'summary', 'program', 'groupname'

    * Flag behaviour: 'opt', 'enum', 'verbosity', 'ignore'

    * Flag name assignment: 'name', 'explicit'

    * Controlling non-flag arguments: 'args', 'argPos'

    * multi-mode programs: 'modes', 'auto'

    /Supported Types/: Each field in the record must be one of the supported
    atomic types (@String@, @Int@, @Integer@, @Float@, @Double@, @Bool@, an
    enumeration, a tuple of atomic types) or a list (@[]@) or @Maybe@ wrapping
    at atomic type.

    /Miss Fields/: If a field is shared by multiple modes, it may be omitted
    in subsequent modes, and will default to the previous value.

    /Purity/: Values created with annotations are not pure - the first
    time they are computed they will include the annotations, but subsequently
    they will not. If you wish to run the above example in a more robust way:

    @sample = 'hawkMode' $ Sample{hello = ...

    @main = print =<< 'hawkRun' sample@

    Even using this scheme, sometimes GHC's optimisations may share values who
    have the same annotation. To disable sharing you may need to specify
    @\{\-\# OPTIONS_GHC -fno-cse \#\-\}@ in the module you define the flags.

    /Pure annotations/: Alternatively, you may use pure annotations, which are
    referentially transparent, but less type safe and more verbose. The initial
    example may be written as:

    @sample = 'record' Sample{} [hello := 'def' '+=' 'help' \"World argument\" '+=' 'opt' \"world\"]@
    @         '+=' 'summary' \"Sample v1\"@

    @main = print =<< (hawk_ sample :: IO Sample)@

    All the examples are written using impure annotations. To convert to pure
    annotations follow the rules:

    > Ctor {field1 = value1 &= ann1, field2 = value2} &= ann2 ==> record Ctor{} [field1 := value1 += ann1, field2 := value2] += ann2
    > Ctor (value1 &= ann1) value2 &= ann2 ==> record Ctor{} [atom value1 += ann1, atom value2] += ann2
    > modes [Ctor1{...}, Ctor2{...}] ==> modes_ [record Ctor1{} [...], record Ctor2{} [...]]
    > Ctor {field1 = enum [X &= ann, Y]} ==> record Ctor{} [enum_ field1 [atom X += ann, atom Y]]

    If you are willing to use TemplateHaskell, you can write in the impure syntax,
    but have your code automatically translated to the pure style. For more details see
    "System.Console.Hawk.Quote".

-}
module System.Console.Hawk.Implicit(

    hawk, hawkMode, hawkRun, hawkApply, Hawk(..),

    module System.Console.Hawk.Implicit.UI,

    (!), build, value, Annotate((:!)),

    module System.Console.Hawk.Verbosity,
    module System.Console.Hawk.Default,
    Extra, Mode,
    Data, Typeable
    ) where

import Data.Data
import Data.Maybe
import Data.Hawk.Any
import System.Exit
import System.Console.Hawk.Explicit(Mode,processArgs,remap,modeReform)
import System.Console.Hawk.Implicit.Extra
import System.Console.Hawk.Annotate
-- import System.Console.Hawk.Annotate hiding ((!))
-- import qualified System.Console.Hawk.Annotate as A((!))
import System.Console.Hawk.Implicit.Type
import System.Console.Hawk.Implicit.Local
import System.Console.Hawk.Implicit.Global
import System.Console.Hawk.Implicit.UI
import System.Console.Hawk.Verbosity
import System.Console.Hawk.Default

-- hawk :: Data a => a -> IO a
-- hawk = hawkRun . hawkMode

hawk :: Data a => Annotate Extra -> IO a
hawk a = hawkRun =<< (hawkMode a)

hawkCapture :: Data a => Capture Extra -> Mode (Hawk a)
hawkCapture = remap embed proj . global . local
    where embed = fmap fromAny
          proj x = (fmap Any x, embed)

hawkMode :: Data a => Annotate Extra -> IO (Mode (Hawk a))
hawkMode a = hawkCapture <$> (capture a)

hawkRun :: Mode (Hawk a) -> IO a
hawkRun m = hawkApply =<< processArgs m

hawkApply :: Hawk a -> IO a
hawkApply Hawk{..}
    | Just x <- hawkHelp = do putStr x; exitSuccess
    | Just x <- hawkVersion = do putStr x; exitSuccess
    | otherwise = do
        maybe (return ()) setVerbosity hawkVerbosity
        return hawkValue
