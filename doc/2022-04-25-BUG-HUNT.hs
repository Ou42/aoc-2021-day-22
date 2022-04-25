-- 2022-04-25
-- test-file: i4.txt == 3 "on" RebootSteps
-- outputs: "Scott's ANSWER: 368998; Correct answer is 321769"

$ cd src
$ ghci
> :l SolutionChoices.hs 

*SolutionChoices> inputText <- readFile $ "../" <> inputFilePath scottImplementation_i4
*SolutionChoices> inputText
"on x=-32..18,y=-49..-2,z=-43..3\non x=-15..35,y=-31..16,z=-9..43\non x=-22..23,y=2..47,z=-35..13\n"

*SolutionChoices> generateRemnant $ parseInputText inputText
[Target [TrgSeg (-22, 23),TrgSeg (  2, 47),TrgSeg (-35, 13)]
,Target [TrgSeg (-22,-16),TrgSeg (  2, -2),TrgSeg (-43,-36)]
,Target [TrgSeg (-22,-16),TrgSeg (-49,  1),TrgSeg (-43,  3)]
,Target [TrgSeg (-32,-23),TrgSeg (-49, -2),TrgSeg (-43,  3)]
,Target [TrgSeg (-15, 18),TrgSeg (  2,-32),TrgSeg (-43,-36)]
,Target [TrgSeg (-15, 18),TrgSeg (-49,  1),TrgSeg (-43,  3)]
,Target [TrgSeg (-15, 18),TrgSeg (  2, -2),TrgSeg (-43,-36)]
,Target [TrgSeg (-15, 18),TrgSeg (-31,  1),TrgSeg (-43,-10)]
,Target [TrgSeg (-15, 23),TrgSeg (  2, 16),TrgSeg ( 14, 43)]
,Target [TrgSeg (-15, 23),TrgSeg (-31,  1),TrgSeg ( -9, 43)]
,Target [TrgSeg ( 24, 35),TrgSeg (-31, 16),TrgSeg ( -9, 43)]]

*SolutionChoices> go = generateRemnant $ parseInputText inputText
*SolutionChoices> map volume go
[103684,-168,16779,22560,-8976,81498,-816,38148,17550,68211,30528]

-- changed Segment.dimension to return:
        abs $ end + 1 - start -- isntead of "end + 1 - start"

Now, the volume returned is: 388918

Something's still off ...

> d1 (Segment.TrgSeg (start, end)) = end + 1 - start
> d2 (Segment.TrgSeg (start, end)) = abs $ end + 1 - start
> d3 (Segment.TrgSeg (start, end)) = length $ [start..end]
> d4 (Segment.TrgSeg (start, end)) = (1+) $ abs $ end - start

                    d1      d2      d3      d4
-------------------------------------------------
TrgSeg (-22, 23)    46      46      46      46
TrgSeg (  2, 47)    46      46      46      46
TrgSeg (-35, 13)    49      49      49      49

TrgSeg (-22,-16)     7       7       7       7 
TrgSeg (  2, -2)    -3       3       0       5  <=== !!!!
TrgSeg (-43,-36) ...
