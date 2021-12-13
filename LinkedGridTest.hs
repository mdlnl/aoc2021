import LinkedGrid
import Test.HUnit

testDLG = fromListOfLists [ [1,2,3,4], [10,20,30,40], [100,200,300,400] ]

testRows = TestCase (assertEqual "rows" (rows testDLG) 3)
testColumns = TestCase (assertEqual "columns" (columns testDLG) 4)

main = do
    runTestText "1" testRows
    runTetsText "2" testColumns