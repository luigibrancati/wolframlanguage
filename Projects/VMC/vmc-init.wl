(* ::Package:: *)

delta=0.1;
Harmosc=Compile[{{x,_Real},{a,_Real}},0.5*x^2-2*a^2*x^2+a, Parallelization->True,CompilationTarget->"C"];//Quiet
\[Psi]=Compile[{{x,_Real},{a,_Real}},Exp[-x^2*a]*Power[(2*a)/\[Pi],1/4], Parallelization->True,CompilationTarget->"C"];//Quiet


metropolisStep=Compile[{{x,_Real},{a,_Real}},
	With[{xtrial=x+delta*RandomChoice[{-1,1}],r=RandomReal[1]}, 
		If[r<(\[Psi][xtrial,a]/\[Psi][x,a])^2, Return[xtrial], Return[x]]
	],
	Parallelization->True, CompilationTarget->"C"];//Quiet
metropolisTotal[initialConf_List,mcsteps_Integer,\[Sigma]_Real]:=NestList[Map[metropolisStep[#,\[Sigma]]&],initialConf,mcsteps];


metropolisInit[initialConf_List, walkers_Integer, mcsteps_Integer, \[Alpha]_Real]:=Module[{acceptance,config},
config=metropolisTotal[initialConf,mcsteps,\[Alpha]];
acceptance=Total[Differences[config]/.n_Real/;n!=0->1,2]/(walkers*mcsteps);
delta*=(acceptance/0.5);
Return[Last@config];
];
metropolisTherm[initialConf_List,walkers_Integer,mcsteps_Integer,\[Alpha]_Real]:=Module[{}, delta=0.1; Return[Nest[metropolisInit[#,walkers,Ceiling[mcsteps*0.1],\[Alpha]]&,initialConf,9]];]
