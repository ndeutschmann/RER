PolyRegionExpand[U_,ASYOutput_,Param_,rho_,Order_]:=Block[
(* Take a polynomial, a list of scaling rules for the Feynman parameters and scaling rules for the parameters, return the LO in the small parameter for each region *)
    {
        Scalings,
        ScaledUs,
        ScaledUsLO,
	RhoMatch,
	PlusElements
    },

    Scalings=Table[ASYOutputRegion[[1]]/.Table[y[i]->x[i]rho^ASYOutputRegion[[3,i]],{i,Length[ASYOutputRegion[[1]]]}],{ASYOutputRegion,ASYOutput}];
    ScaledUs=Table[U/.Scaling,{Scaling,Scalings}]/.Param;

    ScaledUsExpanded=(SortBy[List@@(Collect[#,rho]),Exponent[#,rho]&])&/@ScaledUs; (* Pb: rho^0 terms appear separately in the list *)

    RhoMatch = (Exponent[#1,rho]==Exponent[#2,rho])& ;
    PlusElements = (Plus@@#&)/@#&;

    ScaledUsExpanded=Split[#,RhoMatch]&/@ScaledUsExpanded;
    ScaledUsExpanded = PlusElements/@ScaledUsExpanded;
    
    Return[Part[#,1;;Max[Length[#],Order]]&/@ScaledUsExpanded];
]
