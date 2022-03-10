(* ::Package:: *)

(* ::Title:: *)
(*LexicalCases*)


(* ::Abstract:: *)
(*Utility functions*)


BeginPackage["LexicalCases`Utilities`"]

OptionsJoin::usage = "OptionsJoin[sym_1, ..., sym_i] joins the Options of the sym_i"
ExtractHeads::usage = "ExtractHeads[expr] extracts all heads found in expr"
UnwrapAlternatives::usage = "UnwrapAlternatives[expr] if expr is a list of alternatives return the alternatives"
WrapAlternatives::usage = "WrapAlternatives[expr] if expr is an alternatives retun expr in a list"

Begin["`Private`"]

OptionsJoin[sym__Symbol]:=(Map[Options]/*Apply[Join])[{sym}]

(* expr operations *)

ExtractHeads[expr_] := Cases[expr, h_[___] :> h, {0, Infinity}]

UnwrapAlternatives[{expr_Alternatives}] := expr
UnwrapAlternatives[else_] := else

WrapAlternatives[expr_Alternatives] := {expr}
WrapAlternatives[else_] := else

End[]
EndPackage[]
