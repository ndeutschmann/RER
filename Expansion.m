PolyRegionExpand[U_,ASYOutput_,Param_,rho_,Order_]:=Block[
(* Take a polynomial, a list of scaling rules for the Feynman parameters and scaling rules for the parameters, return the LO in the small parameter for each region *)
    {
        Scalings,
        ScaledUs,
        PowMin,
        NormalizedScaledUs
    },

    (*Put the scaling from ASY as a rule x[i]->rho^k[i] x[i]*)
    Scalings=Table[ASYOutputRegion[[1]]/.Table[y[i]->x[i]rho^ASYOutputRegion[[3,i]],{i,Length[ASYOutputRegion[[1]]]}],{ASYOutputRegion,ASYOutput}];

    (*Rescale the polynomial with: 1) the ASY scaling of the Feynman parameters. 2) The scaling of the external parameters (giving the limit) *)
    ScaledUs=Table[U/.Scaling,{Scaling,Scalings}]/.Param//Expand;

    (*Normalize the polynomial to have its LO in rho be rho^0*)
    PowMin = Exponent[#,rho,Min]&/@ScaledUs;
    (*Tranpose contains List[pol,power], on each of these, Replace the List header by the function that divides the po by rho^power*)
    NormalizedScaledUs = ((#1/rho^#2)&) @@#& /@ Transpose[{ScaledUs,PowMin}];

    (*Expand the polynomial using Series*)
    Return[Normal[Series[#,{rho,0,Order}]]& /@NormalizedScaledUs];



]
