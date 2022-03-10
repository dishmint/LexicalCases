(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Sample strings and expressions for testing*)


BeginPackage["LexicalCases`Utilities`"]

OptionsJoin::usage = "OptionsJoin[sym_1, ..., sym_i] joins the Options of the sym_i"
ExtractHeads::usage = "ExtractHeads[expr] extracts all heads found in expr"

Begin["`Private`"]

OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]


End[]
EndPackage[]
