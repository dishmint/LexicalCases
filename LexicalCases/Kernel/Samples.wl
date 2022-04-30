(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Sample strings and expressions for testing*)


BeginPackage["LexicalCases`Samples`"]

Begin["`Private`"]

(* Samples *)
LexicalCases`$SampleStringExpression = StringExpression[LexicalCases`TextType["Adjective"], " key lime pie"];
(* From https://randomwordgenerator.com/sentence.php *)
LexicalCases`$SampleSentence = "The best key lime pie is still up for debate."
(* From https://randomwordgenerator.com/paragraph.php *)
LexicalCases`$SampleParagraph = "The words hadn't flowed from his fingers for the past few weeks. He never imagined he'd find himself with writer's block, but here he sat with a blank screen in front of him. That blank screen taunting him day after day had started to play with his mind. He didn't understand why he couldn't even type a single word, just one to begin the process and build from there. And yet, he already knew that the eight hours he was prepared to sit in front of his computer today would end with the screen remaining blank."


End[]
EndPackage[]
