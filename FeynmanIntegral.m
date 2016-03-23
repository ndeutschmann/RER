(* ::Package:: *)


FeynmanIntegralUF[U_,F_,Powers_,h_]:= Block[
(* Give the alpha-integrand of a "h"-loop Feynman scalar integral with Symanzik polynomials "U"&"F" and propagator powers "Powers"*)
	{
		a,
		ind,
		count,
		Gammas,
		xProduct
	},

	a=Plus@@Powers;
	(* We only want variables for non-zero propagators. If Powers[i]>0, ind[i] is the number of the corresponding alpha parameter  *)
	ind=Table[0,{i,Length[Powers]}];  
	count=1;
	Do[
		If[
			Powers[[i]]!=0,
			ind[[i]]=count;count=count+1,

			ind[[i]]=-10]
		,{i,Length[Powers]}];

	Gammas = Times@@(Gamma/@Select[Powers,#!=0&]);
	xProduct = Times@@Table[If[Powers[[ii]]!=0,x[ind[[ii]]]^(Powers[[ii]]-1),1],{ii,Length[Powers]}];
	Return[(-1)^a(I Pi^(d/2))^h Gamma[a-h d/2]/Gammas U^(a-(h+1)d/2)/F^(a-h d/2) * xProduct]

]


FeynmanIntegral[LoopMomenta_,Topology_,Replacements_,Powers_]:=Block[
(* Give the alpha-integrand of a Feynman scalar integral in terms of the propagators in "Topology", with propagator powers "Powers" *)
	{
	 a,
	 h,
	 PowersZeroes,
	 myTopo,
	 myPowers,
	 myLoopMomenta,
	 v,
	 U,
	 F
	 },

	 a=Plus@@Powers;
	 h=Length[LoopMomenta];

	 PowersZeroes = Flatten[Position[Powers,0]]; (* Find uselsess propagator positions *)
	 myTopo = Delete[Topology,PowersZeroes]; (* Remove them from the list *)
	 myPowers = Delete[Powers,PowersZeroes]; (* Remove the associated 0 powers *)


	 (* Check which loop momenta remain by listing the variables in myTopo and comparing with LoopMomenta *)
	 v = Variables[myTopo];
	 myLoopMomenta=DeleteCases[
		If[
			MemberQ[v, #], #, Null
		] & /@ LoopMomenta,
		Null
	];

	({U,F,DUMMY}=UF[myLoopMomenta,myTopo,Replacements]);


	Return[FeynmanIntegralUF[U,F,Powers,Length[myLoopMomenta]]]
]
