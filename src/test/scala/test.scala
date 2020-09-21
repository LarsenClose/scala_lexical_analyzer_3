 import org.scalatest.FunSuite
    
 class lexiconAnalyserTests extend FunSuite {
   test("source1") {
     assert(LexicalAnalyzer(source1.dec) === source1output.txt)
   } test("source2") {
     assert(LexicalAnalyzer(source2.dec) === source2output.txt)
   } test("source3") {
     assert(LexicalAnalyzer(source3.dec) === source3output.txt)
   } test("source4") {
     assert(LexicalAnalyzer(source4.dec) === source4output.txt)
   } test("source5") {
     assert(LexicalAnalyzer(source5.dec) === source5output.txt)
   }
 }

