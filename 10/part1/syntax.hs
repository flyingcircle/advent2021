


data Chunk = Paren Chunk | Bracket Chunk | Brace Chunk | Angle Chunk | EmptyChunk
data 

parse :: String -> Maybe Chunk
parse ("(":")":xs) = 
