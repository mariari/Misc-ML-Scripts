import Data.Bits as B
import Data.Word as W
import Data.Int  as I
import Data.Digits

unsignedInt :: Word16
unsignedInt = 8

signedInt :: Int16
signedInt = 8 

binaryList :: [Int16]
binaryList = digits 2 signedInt

removeRightMostBit :: (Bits a, Num a) => a -> a
removeRightMostBit x = x .&. (x - 1)

addRightMostBit :: (Bits a, Num a) => a -> a
addRightMostBit x = x .|. (x + 1)

-- returns 0 if it's 2^n - 1 or 0
allOnes :: (Bits a, Num a) => a -> a
allOnes x = x .&. (x + 1)


posOfRightMostZero :: (Bits a, Num a) => a -> a
posOfRightMostZero x = complement x .&. (x + 1)


onesExceptForFarRightOne :: (Bits a, Num a) => a -> a
onesExceptForFarRightOne x = complement x .|. (x - 1)

oneTrailingZeros :: (Bits a, Num a) => a -> a
oneTrailingZeros x = complement x .&. (x - 1)
