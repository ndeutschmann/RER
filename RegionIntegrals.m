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
        

        ASYOutput=AlphaRepExpand[LoopMomenta, Propagators, Replacements,RegionRules,PreResolve -> True]
	
        
        (* Needs the function developped in the Expansion branch *)
        ExpandedUs=PolyRegionExpand[U,ASYOutput,RegionRules,rho,1];
        ExpandedFs=PolyRegionExpand[F,ASYOutput,RegionRules,rho,1];

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

	LOExponent = Exponent[LORI[LoopMomenta,Propagators,Replacements,Powers,rho,RegionRules],rho];

        {U,F,Dummy}=UF[LoopMomenta,Propagators,Replacements];
        
        (*ASYSTUFF*)
        (*TODO*)
        (*ASYSTUFF*)

      	RegionUFs = Transpose[{ExpandedUs,ExpandedFs}];

        RegionIntegrals = Series[FeynmanIntegralUF[#[[1]],#[[2]],Powers, Length[LoopMomenta]],{rho,LOExponent,LOExponent+1}]&/@RegionUFs; 
        
        Return[RegionIntegrals];
        
        ]
