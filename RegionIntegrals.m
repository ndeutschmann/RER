LORI[LoopMomenta_,Propagators_,Replacements_,Powers_,rho_,RegionRules_]:= 
    Block[
        {
            U,F,Dummy,
            ASYOuput,
            ExpandedUs,
            ExpandedFs,
            RegionsUFs
        },
        {U,F,Dummy}=UF[LoopMomenta,Propagators,Replacements];
        

        ASYOutput=AlphaRepExpand[LoopMomenta, Propagators, Replacements,RegionRules,PreResolve -> True];
	
        
        PRERegionRules = RegionRules /. Rule[a_, b_] :> Rule[a,a*rho^(Exponent[b, x])];

        ExpandedUs=PolyRegionExpand[U,ASYOutput,PRERegionRules,rho,1];
        ExpandedFs=PolyRegionExpand[F,ASYOutput,PRERegionRules,rho,1];

        RegionUFs = Transpose[{ExpandedUs,ExpandedFs}];
        
        RegionIntegrals = FeynmanIntegralUF[#[[1]],#[[2]],Powers,Length[LoopMomenta]]&/@RegionUFs;

        Return[RegionIntegrals];
        
        ]



NLORI[LoopMomenta_,Propagators_,Replacements_,Powers_,rho_,RegionRules_]:= 
    Block[
        {
	    LOExponent,
            U,F,Dummy,
            ASYOuput,
            ExpandedUs,
            ExpandedFs,
            RegionsUFs,
	    RegionIntegrals
        },

	LOExponents = Simplify[Exponent[#,rho]&/@Flatten[PowerExpand/@LORI[LoopMomenta,Propagators,Replacements,Powers,rho,RegionRules]]];

        {U,F,Dummy}=UF[LoopMomenta,Propagators,Replacements];
        
        
        ASYOutput=AlphaRepExpand[LoopMomenta, Propagators, Replacements,RegionRules,PreResolve -> True];
	Scalings=Table[ASYOutputRegion[[1]]/.Table[y[i]->x[i]rho^ASYOutputRegion[[3,i]],{i,Length[ASYOutputRegion[[1]]]}],{ASYOutputRegion,ASYOutput}];
  	RhoRegionRules = RegionRules /. Rule[a_, b_] :> Rule[a,a*rho^(Exponent[b, x])];
	ScaledUs=Simplify[Table[U/.Scaling,{Scaling,Scalings}]/.RhoRegionRules];
	ScaledFs=Simplify[Table[F/.Scaling,{Scaling,Scalings}]/.RhoRegionRules];

        NonZero[rho_,i_]:=If[i===0,1,rho^i];
        
     (*   RegionIntegrals =  Table[Simplify[FeynmanIntegralUF[ScaledUs[[i]],ScaledFs[[i]],Powers, Length[LoopMomenta]]/(NonZero[rho,LOExponents[[i]]])],{i,Length[ScaledUs]}];*)
        RegionIntegrals =  Table[Series[FeynmanIntegralUF[ScaledUs[[i]],ScaledFs[[i]],Powers, Length[LoopMomenta]]/(NonZero[rho,LOExponents[[i]]])//PowerExpand//Simplify,{rho,0,1}],{i,Length[ScaledUs]}];
        Return[RegionIntegrals];
        
        ]
