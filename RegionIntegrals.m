LORI[LoopMomenta_,Propagators_,Replacements_,Powers_,rho_,RegionRules_]:=
    Block[
        {
            U,F,Dummy,
            ASYOuput,
            ExpandedUs,
            ExpandedFs,
            RegionsUFs,
            RegionIntegrals,
            PRERegionRules
        },
        {U,F,Dummy}=UF[LoopMomenta,Propagators,Replacements];


        ASYOutput=AlphaRepExpand[LoopMomenta, Propagators, Replacements,RegionRules,PreResolve -> True];


        PRERegionRules = RegionRules /. Rule[a_, b_] :> Rule[a,a*rho^(Exponent[b, x])];

        ExpandedUs=PolyRegionExpand[U,ASYOutput,PRERegionRules,rho,0];
        ExpandedFs=PolyRegionExpand[F,ASYOutput,PRERegionRules,rho,0];

        RegionUFs = Transpose[{ExpandedUs,ExpandedFs}];

        RegionIntegrals = FeynmanIntegralUF[#[[1]],#[[2]],Powers,Length[LoopMomenta]]&/@RegionUFs;

        Return[RegionIntegrals];

]

RegionExpand[LoopMomenta_,Propagators_,Replacements_,Powers_,rho_,RegionRules_,Pow_]:=
    Block[
        {
          LOExponent,
          U,F,Dummy,
          ASYOuput,
          Scalings,
          PRERegionRules,
          ExpandedUs,
          ExpandedFs,
	        RegionIntegrals
        },

      LOExponents = Simplify[Exponent[#,rho]&/@Flatten[PowerExpand/@LORI[LoopMomenta,Propagators,Replacements,Powers,rho,RegionRules]]];

      {U,F,Dummy}=UF[LoopMomenta,Propagators,Replacements];

      ASYOutput=AlphaRepExpand[LoopMomenta, Propagators, Replacements,RegionRules,PreResolve -> True];
	    Scalings=Table[ASYOutputRegion[[1]]/.Table[y[i]->x[i]rho^ASYOutputRegion[[3,i]],{i,Length[ASYOutputRegion[[1]]]}],{ASYOutputRegion,ASYOutput}];
      PRERegionRules = RegionRules /. Rule[a_, b_] :> Rule[a,a*rho^(Exponent[b, x])];

	    (*ScaledUs=Simplify[Table[U/.Scaling,{Scaling,Scalings}]/.RhoRegionRules];*)
	    (*ScaledFs=Simplify[Table[F/.Scaling,{Scaling,Scalings}]/.RhoRegionRules];*)

      ExpandedUs=PolyRegionExpand[U,ASYOutput,PRERegionRules,rho,Pow];
      ExpandedFs=PolyRegionExpand[F,ASYOutput,PRERegionRules,rho,Pow];

      NonZero[rhu_,i_]:=If[i===0,1,rhu^i];

      RegionIntegrals =  Table[Series[FeynmanIntegralUF[ExpandedUs[[i]],ExpandedFs[[i]],Powers, Length[LoopMomenta]]/(NonZero[rho,LOExponents[[i]]])//PowerExpand//Simplify,{rho,0,Pow}],{i,Length[ExpandedUs]}];
      Return[RegionIntegrals];

]
