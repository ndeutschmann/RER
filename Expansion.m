LOPolyRegionExpand[U_,ASYOutput_,Param_]:=Block[
(* Take a polynomial, a list of scaling rules for the Feynman parameters and scaling rules for the parameters, return the LO in the small parameter for each region *)
    {
        Scalings,
        ScaledUs,
        ScaledUsLO
    }

    Scalings=Table[ASYOutputRegion[[1]]/.Table[y[i]->x[i]\[Rho]^ASYOutputRegion[[3,i]],{i,Length[ASYOutputRegion[[1]]]}],{ASYOutputRegion,ASYOutput}];
    ScaledUs=Table[U/.Scaling,{Scaling,Scalings}]/.Param;
    ScaledUsLO=(If[Exponent[#,1/\[Rho]]!=0,Coefficient[#,1/\[Rho]^(Exponent[#,1/\[Rho]])],#/.\[Rho]->0])&/@ScaledUs

    Return[ScaledUsLO]
]
