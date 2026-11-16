-- | What the scenes of "Test.Scorecard" came to by the old tracker, to
-- hold it to tracking as it did.
module Test.Old.Scorecard (limits) where

-- | The most each scene's card may come to by the old tracker, by the
-- scene's name: frames lost with the clip showing, tracked with it
-- absent, wrong, and mirrored; the worst error in degrees; and the
-- longest unfound, in seconds.
--
-- Where tracking slips in "nodding through square-on, the user's clip"
-- turns on the last bit of the arithmetic: Windows loses 279 frames and
-- has 105 mirrored where Linux loses 161 and has 29. The limits take in
-- both.
limits :: [(String, (Int, Int, Int, Int, Double, Double))]
limits =
    [ ("nodding through where labels by height swap", (0, 0, 0, 0, 0.5, 0))
    , ("a jump of 40 degrees", (4, 0, 0, 0, 0.5, 0.04))
    , ("the top reflector fading for a moment", (0, 0, 0, 0, 0.9, 0))
    , ("the top reflector gone for two seconds", (0, 0, 0, 0, 1, 0))
    , ("a stray reflection while a reflector fades", (0, 0, 0, 0, 1.6, 0))
    , ("lights by the camera laid out like the clip", (0, 0, 0, 0, 0.5, 0))
    , ("leaning in to 270 mm", (0, 0, 0, 0, 0.5, 0))
    , ("sat back half a metre after a break", (0, 0, 0, 0, 0.4, 0))
    , ("a phone in front of the clip in view", (0, 0, 0, 0, 0.5, 0))
    , ("a phone in front of where the clip was", (0, 0, 0, 0, 0.5, 0))
    , ("a reflector in pieces and a fleck by another", (0, 0, 0, 0, 1.1, 0))
    , ("rolling over until the top reflector isn't highest", (0, 0, 0, 0, 1.9, 0))
    , ("turned far, the top reflector running into a lower one", (0, 0, 0, 0, 2.6, 0))
    , ("back, turned, with the top reflector faint", (0, 0, 0, 0, 0.5, 0))
    , ("a slow sweep, the bent clip", (0, 0, 0, 0, 2.2, 0))
    , ("a slow sweep, the straight clip", (0, 0, 0, 0, 1.7, 0))
    , ("a frame of the clip where it was 15 degrees back, in a fast turn", (4, 0, 3, 0, 17.3, 0.04))
    , ("a leap of 40 degrees while glancing", (4, 0, 0, 0, 0.7, 0.04))
    , ("the clip shown elsewhere for three frames", (0, 0, 5, 0, 31.3, 0))
    , ("a frame of the clip turned away, two reflectors' places still lit", (3, 0, 0, 0, 0.7, 0.03))
    , ("a light by a reflector gone dark for a moment", (0, 0, 8, 0, 8.3, 0))
    , ("a lamp coming on beside a glancing head", (0, 0, 0, 0, 0.5, 0))
    , ("a slow sweep at 60 frames a second, the user's clip", (0, 0, 0, 0, 2.1, 0))
    , ("a slow sweep at 60 frames a second, the top tab unbent", (0, 0, 0, 0, 2, 0))
    , ("a slow sweep at 60 frames a second, a scalene clip", (0, 0, 0, 0, 2.1, 0))
    , ("nodding through square-on, the top tab unbent", (0, 0, 6, 115, 22, 0))
    , ("nodding through square-on, the user's clip", (300, 0, 185, 115, 28.2, 0.86))
    , ("looking up 30 degrees while turning far, the user's clip", (0, 0, 0, 0, 2.3, 0))
    , ("looking up 30 degrees while turning far, points", (0, 0, 0, 0, 3.2, 0))
    , ("looking up, only the lower pair in view for 20 seconds", (0, 0, 80, 0, 19.8, 0))
    , ("a light as big as a reflector by one gone dark for half a second", (0, 0, 242, 0, 23, 0))
    , ("quick glances, 280 degrees a second, at 60 frames a second", (0, 0, 0, 0, 0.6, 0))
    , ("the fastest glances, 550 degrees a second", (0, 0, 0, 0, 0.5, 0))
    , ("a hand passing over each reflector in turn", (0, 0, 0, 0, 3.4, 0))
    , ("seen first among two strays as big as reflectors", (381, 0, 0, 0, 0.2, 3.02))
    , ("a still light as big as a reflector while another is hidden for half a second", (254, 0, 381, 0, 182.8, 2.02))
    , ("two reflectors and a stray, the clip not seen before", (0, 0, 0, 0, 0.5, 0))
    , ("two strays coming on beside a glancing head, lights not masked", (0, 0, 0, 0, 0.7, 0))
    , ("glancing with centres a pixel out", (0, 0, 0, 0, 4.1, 0))
    , ("leaving the picture at its edge, and back", (0, 0, 0, 0, 0.8, 0))
    , ("glancing by a lit fire", (381, 0, 0, 0, 0.5, 3.03))
    , ("glancing by a lit fire, lights not masked", (1263, 0, 0, 0, 0.2, 10.02))
    , ("a fleck by the right reflector as the other two run together", (4, 0, 21, 0, 3.2, 0.04))
    , ("a fleck 3 pixels across amid the reflectors, steady, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 5 pixels across amid the reflectors, steady, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across amid the reflectors, steady, the head still", (381, 0, 0, 0, 0.6, 3.03))
    , ("a fleck 3 pixels across amid the reflectors, flickering, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 5 pixels across amid the reflectors, flickering, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across amid the reflectors, flickering, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 3 pixels across beside a reflector, steady, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 5 pixels across beside a reflector, steady, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across beside a reflector, steady, the head still", (381, 0, 0, 0, 0.6, 3.03))
    , ("a fleck 3 pixels across beside a reflector, flickering, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 5 pixels across beside a reflector, flickering, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across beside a reflector, flickering, the head still", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 3 pixels across amid the reflectors, steady, the head glancing", (0, 0, 0, 0, 0.9, 0))
    , ("a fleck 5 pixels across amid the reflectors, steady, the head glancing", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across amid the reflectors, steady, the head glancing", (3, 0, 0, 0, 0.6, 0.03))
    , ("a fleck 3 pixels across amid the reflectors, flickering, the head glancing", (0, 0, 0, 0, 0.7, 0))
    , ("a fleck 5 pixels across amid the reflectors, flickering, the head glancing", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across amid the reflectors, flickering, the head glancing", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 3 pixels across beside a reflector, steady, the head glancing", (0, 0, 0, 0, 0.7, 0))
    , ("a fleck 5 pixels across beside a reflector, steady, the head glancing", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across beside a reflector, steady, the head glancing", (3, 0, 0, 0, 0.6, 0.03))
    , ("a fleck 3 pixels across beside a reflector, flickering, the head glancing", (0, 0, 0, 0, 0.7, 0))
    , ("a fleck 5 pixels across beside a reflector, flickering, the head glancing", (0, 0, 0, 0, 0.6, 0))
    , ("a fleck 7 pixels across beside a reflector, flickering, the head glancing", (0, 0, 0, 0, 0.6, 0))
    ]
