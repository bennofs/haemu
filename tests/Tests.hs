import           HaemuInstruction
import           HaemuMonad
import           Test.Hspec


main = hspec $ do

  describeHaemuMonad
  describeInstructionModule
