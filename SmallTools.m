(* Tool for sectorization *)
(NonZero[n_] :=If[n > 0, 1, 0]);

(*List of propagators in the integral*)
(Explicit[Int_,Propagators_] := Table[Propagators[[i]]^NonZero[Int[[i]]], {i, 1, Length[Propagators]}] // DeleteCases[#, 1] &); 

(* From a "topology" input (list of integers), generate the associated sector *)
(Sectorize[Int_]:= NonZero/@Int);

(* Apply Chen's Theorem to variable i in a Feynman representation integral *)
(ChenTheorem[Int_, i_] := (Int /. x[i] -> 1) // FullSimplify);

(* Integrate an integral in Feynman representation after the application of Chen's Theorem to 1 variable *)
(IntegrateAlpha[Int_, i_] := Integrate[Int, {x[i], 0, Infinity}, GenerateConditions -> False]);

(* Change variable x[i]->x[i]*x[j] in a Feynman integral after Chen's theorem *)
(RescaleInt[Int_, i_, j_] := (Int /. x[i] -> x[i] x[j])*x[j] //Simplify);

(* Generate conditions for variables *)
(GenerateAssumptions[x__]:=And @@ (# > 0 & /@ List[x]));


(* Normalization factor for loops *)
(LoopFactor = I Pi^(d/2) Exp[-ep EulerGamma])


