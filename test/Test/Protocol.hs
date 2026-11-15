-- | Commands to the camera, hidden among padding.
module Test.Protocol (tests) where

import Data.Bits ((.&.))
import qualified Data.ByteString as B
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding ((.&.))

import TrackKat.Protocol

tests :: TestTree
tests =
    testGroup
        "Protocol"
        [ testProperty "a command is 24 bytes, whatever the padding" $
            forAllShrink genCommand shrinkCommand $ \cmd seed ->
                B.length (fst (encode (newSeed seed) cmd)) === 24
        , testProperty "a command is read back as sent, whatever the padding" $
            forAllShrink genCommand shrinkCommand $ \cmd seed ->
                decode (arity cmd) (fst (encode (newSeed seed) cmd)) === Just (asSent cmd)
        ]

-- | How many bytes a command carries, which says how it is read back.
arity :: Command -> Int
arity Cmd1{} = 1
arity Cmd2{} = 2
arity Cmd4{} = 4

-- | A command as the camera reads it: of a two-byte command's argument,
-- only the low four bits are sent.
asSent :: Command -> Command
asSent (Cmd2 op v) = Cmd2 op (v .&. 0x0F)
asSent cmd = cmd

genCommand :: Gen Command
genCommand = oneof [Cmd1 <$> arbitrary, Cmd2 <$> arbitrary <*> arbitrary, Cmd4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

-- | Commands with smaller bytes, and with fewer.
shrinkCommand :: Command -> [Command]
shrinkCommand (Cmd1 op) = Cmd1 <$> shrink op
shrinkCommand (Cmd2 op v) = Cmd1 op : [Cmd2 op' v' | (op', v') <- shrink (op, v)]
shrinkCommand (Cmd4 op a b c) = Cmd2 op a : [Cmd4 op' a' b' c' | (op', a', b', c') <- shrink (op, a, b, c)]
