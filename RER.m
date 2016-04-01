If[ValueQ[RERPath],Null,RERPath = NotebookDirectory[]]

SetDirectory[RERPath]

Get[RERPath<>"asy2.1.m"]
Get[RERPath<>"SmallTools.m"]
Get[RERPath<>"FeynmanIntegral.m"]
Get[RERPath<>"Expansion.m"]
Get[RERPath<>"RegionIntegrals.m"]
