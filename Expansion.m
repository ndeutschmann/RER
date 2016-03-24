PolyRegionExpand[U_,ASYOutput_,Param_,rho_,Order_]:=Block[
(* Take a polynomial, a list of scaling rules for the Feynman parameters and scaling rules for the parameters, return the LO in the small parameter for each region *)
    {
        Scalings,
        ScaledUs,
        ScaledUsLO
    }

    Scalings=Table[ASYOutputRegion[[1]]/.Table[y[i]->x[i]rho^ASYOutputRegion[[3,i]],{i,Length[ASYOutputRegion[[1]]]}],{ASYOutputRegion,ASYOutput}];
    ScaledUs=Table[U/.Scaling,{Scaling,Scalings}]/.Param;
    
    ScaledUsExpanded=(SortBy[List@@(Collect[#,rho]),Exponent[#,rho]&])&/@ScaledUs;
    
    Return[Part[#,1;;Order]&/@ScalesUsExpanded]
]
