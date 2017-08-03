import LongDiv

chkConditions longDivRes = foldl (&&) True $
    [ length longDivRes == 3
    , countDigits (unpck4 (longDivRes !! 0) 1) == 5
    , countDigits (unpck4 (longDivRes !! 1) 0) == 5
    , countDigits (unpck4 (longDivRes !! 1) 1) == 4
    , countDigits (unpck4 (longDivRes !! 2) 0) == 4
    , countDigits (unpck4 (longDivRes !! 2) 2) == 2
    , all (\x -> any (x==) [3,1,6,2,9]) $ take 2 $ n2digs (unpck4 (longDivRes !! 0) 1)
    , all ((unpck4 (longDivRes !! 1) 3) /=) [0,5,6]
    , (unpck4 (longDivRes !! 0) 3) /= 0
    , (unpck4 (longDivRes !! 2) 3) /= 0
    , all ((unpck4 (longDivRes !! 2) 3) /=) [0,5,7,9]
    , any ((n2digs (unpck4 (longDivRes !! 1) 0)) !! 3 ==) [3,1,6,2,9]
    , any ((n2digs (unpck4 (longDivRes !! 1) 0)) !! 4 ==) [4,1,8,2]
    , any ((last $ n2digs (unpck4 (longDivRes !! 1) 1)) ==) [8,1]
    , any ((last $ n2digs (unpck4 (longDivRes !! 2) 0)) ==) [7,1]
    , any ((n2digs (unpck4 (longDivRes !! 2) 0)) !! 2 ==) [0,1,6]
    , all ((last $ n2digs (unpck4 (longDivRes !! 2) 1)) /=) [0,5]
    , any ((head $ n2digs (unpck4 (longDivRes !! 2) 2)) ==) [3,6,9,1,2]
    , (last $ n2digs (unpck4 (longDivRes !! 2) 2)) /= 6
    ]


chkDivision (dividend, divisor, longDivRes) = foldl (&&) True $
    [ ddvi !! 2 == 1
    , (last $ n2digs (unpck4 (longDivRes !! 2) 3)) == 2
    , ddvd !! 3 == 5
    , ddvd !! 4 == 6
    , ddvd !! 5 == 4
    , ddvd !! 6 == 7
    , (n2digs (unpck4 (longDivRes !! 0) 1)) !! 0 == 3
    , (n2digs (unpck4 (longDivRes !! 0) 1)) !! 1 == 3
    , (n2digs (unpck4 (longDivRes !! 1) 0)) !! 0 == 1
    , (n2digs (unpck4 (longDivRes !! 1) 0)) !! 3 == 3
    , (n2digs (unpck4 (longDivRes !! 1) 0)) !! 4 == 4
    , (last $ n2digs (unpck4 (longDivRes !! 1) 1)) == 8
    , (last $ n2digs (unpck4 (longDivRes !! 2) 0)) == 7
    , (head $ n2digs (unpck4 (longDivRes !! 2) 2)) == 3
    ] where
        ddvi = n2digs divisor
        ddvd = n2digs dividend


trip dividend divisor = (dividend, divisor, longDivision dividend divisor)


testJSCsolution = do

    let p1 = trip 3445681 4779
    let p2 = trip 2071127 3277
    print $ p1
    print $ chkConditions (last3 p1)
    print $ p2
    print $ chkConditions (last3 p2)

    let d  = chkDivision $ applyToTestRes q p1 p2
        q a b | (a /= 0) && (b /= 0) = max (quot a b) (quot b a)
              | (a == 0) && (b == 0) = 1
              | otherwise        = 9

    print d


test1 = map snd $ filter fst (map chkOne sq)
    where
        dividendGen [a,b,c,d,e,f,g] = digs2n [a,b,c,d,e,f,g]
        divisorGen [a,b,c,d] = digs2n [a,b,c,d]
        zeroToNine n = replicate n [0..9]
        sq = sequence $ [[1..9]] ++ (zeroToNine 3) ++ [[1..9]] ++ (zeroToNine 2) ++ [[1,5]] ++ [[1,6]] ++ [[1,2,4,8]] ++ [[7,1]]

        chkOne x = (chkConditions ldr, (dividend, divisor, ldr))
            where
                ldr      = longDivision dividend divisor
                divisor  = divisorGen (take 4 x)
                dividend = dividendGen (drop 4 x)


opDigitWise op a b = digs2n $ zipWith op (n2digs a) (n2digs b)

opTupleWise op (a,b,c,d) (a',b',c',d') = (op a a', op b b', op c c', op d d')

applyToLdr op ldr1 ldr2 = zipWith (opTupleWise op) ldr1 ldr2

applyToTestRes op tr1 tr2 = ttr (opDigitWise op) tr1 tr2
    where ttr op (a, b, c) (a', b', c') = (op a a', op b b', applyToLdr op c c')


filterGood test = filter ((/=[]) . snd) $ map fg (zip [1..length test] test)
    where fg (n, tr) = (tr, filter (chkDivision . aDiv tr) (drop n test))
          aDiv  = applyToTestRes q
          q a b | (a /= 0) && (b /= 0) = max (quot a b) (quot b a)
                | (a == 0) && (b == 0) = 1
                | otherwise            = 9

main = do
    --testJSCsolution
    mapM_ print (filterGood test1)
