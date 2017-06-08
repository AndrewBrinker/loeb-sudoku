Welcome to the final lab for the spring 2017 session of CSE 320, Programming
Languages! In this final lab, you do not need to do an any work yourself, nor
turn anything in. All you need to do is sign the sign in sheet which is making
its way around the room.

In today's lab I am going to tell you a story with code, which I hope you will
find interesting. You do not need to take notes; just listen.

(Just getting the import out of the way)

> import Data.List


There once was a small village, in which lived a great many people who were,
unfortunately, quite poor at puzzles. One day in this town, a great wizard
arrived, carrying with him a spellbook filled with magic. The townsfolk
gathered in the square on word that the wizard was there, to see perhaps some
of the magic he held.

"Hello! I have come to your town to perform great feats of magic!" said the
wizard. "I shall need a volunteer from the crowd!"

The wizard looked around, scanning the audience until his eyes settled on a
Sudoku puzzle in the back.

"You there! Puzzle! Will you be my assistant today?"

The puzzle looked up (hey man, it's a magic world, the puzzle is sentient)
and then made its way to the center of the group. On arriving, the wizard
rotated the puzzle this way and that, to make sure all the crowd could see
whom had been chosen. The puzzle looked like this:

> puzzle = [ 3, 4, 2, 0
>          , 0, 1, 3, 4
>          , 1, 0, 4, 3
>          , 4, 3, 0, 2 ]
>
> puzzleSize = 4

"Puzzle, I see you are unsolved!" said the wizard.

"Yes, this town is quite poor as puzzles, you see. No one yet has solved
me." replied the puzzle.

"Well, today for my show I shall do something better than solve you. I shall
use my magic to trick you into solving yourself!" exclaimed the wizard.

At this the townspeople gasped, for who had ever heard of a puzzle solving
itself? The puzzle as well seemed wary, but after a pause, acquiesced to
begin the magical demonstration.

"So, I see you are a board of numbers, where no number repeats in any row
or column." (This is not quite the definition of Sudoku, but it is close
enough for our purposes here.) "I would like to say that maybe we can
think about you a different way."

"What would that way be?" asked the puzzle.

"Well, what if instead of numbers, you were a board of functions? Functions
which take in the entire board itself, and return numbers. I have, in
fact, a spell just for that:"

> process :: (Num a, Eq a, Ord a) => [a] -> [[a] -> a]
> process lst = zipWith valToFunc lst [0..]
>     where valToFunc val idx =
>               if val == 0
>               then holeAt idx
>               else const val

"So now, every number that is already filled in is replaced with a function
which ignores its input and returns the original number. And every number
which is not already filled in is replaced with something else."

"What is that something else?" the puzzle queried.

"Well, it is a function to calculate the missing value!" replied the wizard.

"There is a bit of work to it, but this is the spell for that:"

> holeAt :: (Num a, Eq a, Ord a) => Int -> [a] -> a
> holeAt index = findVal indices
>     where row         = index `div` puzzleSize
>           col         = index `mod` puzzleSize
>           rowIndices  = [puzzleSize * row + i | i <- [0..puzzleSize - 1]]
>           colIndices  = [puzzleSize * i + col | i <- [0..puzzleSize - 1]]
>           indices     = flip (\\) [index] . nub $ rowIndices ++ colIndices
>           missingVals = (\\) [1, 2, 3, 4]
>           valsAt      = flip $ map . (!!)
>           findVal i   = head . missingVals . nub . (valsAt i)
>

"You see, this collects all the values in the row and column of the missing
value, and finds the value that's missing."

The puzzle nodded, but didn't really understand.

"All we need now is a spell to evaluate those functions, and we will have
returned to us a solved board. Pardon me, but I am good at puzzles, and
I believe the solved board should look like this:"

> solution = [ 3, 4, 2, 1
>            , 2, 1, 3, 4
>            , 1, 2, 4, 3
>            , 4, 3, 1, 2 ]

The puzzle looked at the board. "That's correct!" the puzzle shouted.

"Of course it is! And so, with just one incantation, you will be solved,
without any work on my part to solve you!"

> main = do
>     let attempt = loeb . process $ puzzle
>     if solution == attempt
>     then putStrLn $ "Success:  " ++ show attempt
>                ++ "\nExpected: " ++ show solution
>     else putStrLn $ "Failed:   " ++ show attempt
>                ++ "\nExpected: " ++ show solution

The crowd gasped!

"It's solved!" yelled a man.
"He did it!" yelled a woman.

The wizard smiled. The puzzle was elated.

"I'm solved! I'm solved! I thought I'd never be solved!"

The whole crowd was amazed and enthused at the magic of the wizard, which
they didn't understand, but which gave them an excellent show.

A mustachioed man emerged from the crowd. He was the mayor, and he offered
the wizard free lodging in the town inn that night, out of thanks for the
show, and for finally solving the town puzzle.

The wizard happily accepted, and collected his things to take to the inn.

As he left, a young child from the crowd, who had watched the whole show,
followed. The child was inquisitive, and yearned to understand the final
incantation with which the wizard had solved the puzzle.

The child followed the wizard as he entered the inn, set down his
belongings, and exited for the pub. Seizing the moment, the child scurried
into the wizard's room, where there lay on the bed the spellbook from which
the wizard's incantations came.

The child had heard the name of the spell before, and scanned now through
the book for just that name.

Finally, the child found it, and was immediately confused, for it seemed
nonsensical:

> loeb :: Functor f => f (f a -> a) -> f a
> loeb x = go where go = fmap ($ go) x

"Yes! It is a tricky spell indeed!"

The child hadn't heard, but the wizard had returned already, brew apparently
finished. The wizard smiled and set down his hat, approaching the child.

"May I?" the wizard asked, hand outstretched.

The child handed the spellbook to the wizard, who didn't put it away, but sat
down next in a chair beside the child.

"This is one of my favorite spells. I can explain it, if you'd like."

"Yes! Oh yes! Explain it! I have to know how it works!" screamed the child.

"Well, are you familiar with fmap?" queried the wizard.

"Of course, you use it to apply a function over a context." replied the child.

"Right, well, what this does is take in a context filled with functions, like
the board in the puzzle earlier. It fmaps over those functions, applying
(and this is the crazy part) itself on each element, and passing the overall
context to that element as a parameter! But of course, fmap on a function is
simply function composition! And when there's nothing left to be composed, the
result evaluates. So, each function can depend on the overall context, and so
long as there is a path to answer each function (that is, they are not
infinitely referential, either to themselves, or mutually), the overall
result will be computed!"

This was a long explanation, and the child was puzzled by it. They gave the
wizard a look of confusion.

"Ah, well, maybe think on it for a little while."

And with that, the wizard ushered the child out of the room, for it was getting
dark, and the wizard wanted to sleep.

The child walked home that night, and went to sleep in their own bed. But they
continued to ponder the explanation the wizard had given. The puzzle was turned
into a board of functions. But some of the functions already had solutions, they
simply returned a value already known. And the others depended on those known
values, sure, but it would all calculate out in the end. So the incantation
could work!

With that the child, satisfied in their answer, went to sleep.

