--"Key: god"
--"Decipher : (The Gospel of John, chapter 1) 1 In the beginning the Word already existed. He was with God, and he was God. 2 He was in the beginning with God. 3 He created everything there is. Nothing exists that he didn't make. 4 Life itself was in him, and this life gives light to everyone. 5 The light shines through the darkness, and the darkness can never extinguish it. 6 God sent John the Baptist 7 to tell everyone about the light so that everyone might believe because of his testimony. 8 John himself was not the light; he was only a witness to the light. 9 The one who is the true light, who gives light to everyone, was going to come into the world. 10 But although the world was made through him, the world didn't recognize him when he came. 11 Even in his own land and among his own people, he was not accepted. 12 But to all who believed him and accepted him, he gave the right to become children of God. 13 They are reborn! This is not a physical birth resulting from human passion or plan, this rebirth comes from God.14 So the Word became human and lived here on earth among us. He was full of unfailing love and faithfulness. And we have seen his glory, the glory of the only Son of the Father."
--"Solution: 107359"
--
--real	0m0.337s
--user	0m0.329s
--sys	0m0.006s

import Data.List (isInfixOf)
import Data.Char (chr, ord)
import Data.Bits

main = do
        cipherFile <- readFile "Desktop/p059_cipher.txt"
        let cipher = read ("[" ++ cipherFile ++ "]") :: [Int]
            decipher = decrypt cipher
            key = map chr . take 3 $ zipWith xor cipher decipher
        mapM_ print ["Key: " ++ key, "Decipher: " ++ (map chr decipher), "Solution: " ++ show (sum decipher)]

-- [116,104,101,32] stands for "the ", which is a probable word.
decrypt cipher = head .
                 filter ([116,104,101,32] `isInfixOf`) .
                 filter (all seemGood) .
                 map (zipWith xor cipher . cycle) $ keys
keys = [[a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122]]
seemGood n = n >= 32