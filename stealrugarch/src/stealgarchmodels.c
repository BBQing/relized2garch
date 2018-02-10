/*################################################################################
##
##   R package rugarch by Alexios Ghalanos Copyright (C) 2008-2015.
##   This file is part of the R package rugarch.
##
##   The R package rugarch is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   The R package rugarch is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
#################################################################################*/
# include <R.h>
# include <math.h>
# include "stealrugarch.h"
# include "stealfilters.h"
# include "distributions.h"
# include "filters.h"


void real2garchfilterC(int *model, double *pars, int *idx, double *hEst, double *x, double *res,
		double *mexdata, double *vexdata, double *zrf, double *constm, double *condm,
		int *m, int *T, double *h, double *z, double *taup,  double *taum, double *rvsp, double *rvsm, double *up,double *um, double *llh,
		double *LHT1P, double *LHT)
{
	int i;
	double lk=0;
	double hm = 0;
	double detS = pars[idx[29]]*pars[idx[30]]-pars[idx[31]]*pars[idx[31]];
	for(i=0; i<*m; i++)
	{
		h[i] = *hEst;
		arfimaxfilter(model, pars, idx, x, res, mexdata, zrf, constm, condm, sqrt(fabs(*hEst)), *m, i, *T);
		z[i] = res[i]/sqrt(fabs(h[i]));
		taup[i] = pars[idx[25]]*z[i] +  pars[idx[26]]*(z[i]*z[i]-1);
		taum[i] = pars[idx[27]]*z[i] +  pars[idx[28]]*(z[i]*z[i]-1);
		up[i] = log(rvsp[i])-pars[idx[21]] - pars[idx[23]]*log(h[i]) - taup[i];
		um[i] = log(rvsm[i])-pars[idx[22]] - pars[idx[24]]*log(h[i]) - taum[i];
		LHT1P[i] = log(garchdistribution(z[i], sqrt(fabs(h[i])), pars[idx[15]], pars[idx[16]], pars[idx[17]], model[20]));
		LHT[i] = LHT1P[i]  +log(exp((up[i]*up[i]*pars[idx[30]]+um[i]*um[i]*pars[idx[29]]-2*um[i]*up[i]*pars[idx[31]])/detS  )/detS);/* tady se musi pridat uprava pro loglikelihood dmvtnorm*/
		lk = lk - LHT[i];
	}
	for (i=*m; i<*T; i++)
	{
		real2garchfilter(model, pars, idx, res, z, vexdata, *T, i, h, rvsp, rvsm, taup, taum, up, um);
		hm = sqrt(fabs(h[i]));
		arfimaxfilter(model, pars, idx, x, res, mexdata, zrf, constm, condm, hm, *m, i, *T);
		z[i] = res[i]/sqrt(fabs(h[i]));
		LHT1P[i] = log(garchdistribution(z[i], sqrt(fabs(h[i])), pars[idx[15]], pars[idx[16]], pars[idx[17]], model[20]));
		LHT[i] = LHT1P[i] +log(exp((up[i]*up[i]*pars[idx[30]]+um[i]*um[i]*pars[idx[29]]-2*um[i]*up[i]*pars[idx[31]])/detS  )/detS); /*toto je nejake maticove nasobeni */
		lk = lk - LHT[i];
	}
	*llh=lk;
}



void real2garchsimC(int *model, double *pars, int *idx, double *res, double *vexdata, int *m,
		int *T, double *h, double *z, double *taup, double *taum, double *rsvp, double *rsvm, double *up, double *um)
{
	int i;
	for (i=*m; i<*T; i++)
	{
		int j, ind;
		h[i] = h[i] +  pars[idx[6]];
		if( model[14]>0 )
		{
			for( j=0; j<model[14]; j++ )
			{
				ind = i + ( *T * j );
				h[i] = h[i] + pars[idx[14]+j]*vexdata[ind];
			}
		}
		for( j=0; j<model[19]; j++ )
		{
			h[i] = h[i] + pars[idx[19]+j]*log(rsvp[i-(j+1)]);
		}
		for( j=0; j<model[20]; j++ )
		{
			h[i] = h[i] + pars[idx[20]+j]*log(rsvm[i-(j+1)]);
		}
		for( j=0; j<model[8]; j++ )
		{
			h[i] = h[i] + pars[idx[8]+j]*log(h[i-(j+1)]);
		}
		h[i] = exp(h[i]);
		taup[i] = pars[idx[25]]*z[i] +  pars[idx[26]]*(z[i]*z[i]-1);
		taum[i] = pars[idx[27]]*z[i] +  pars[idx[28]]*(z[i]*z[i]-1);
		rsvp[i] = exp(pars[idx[21]] + pars[idx[23]]*log(h[i]) + taup[i] + up[i]);
		rsvm[i] = exp(pars[idx[22]] + pars[idx[24]]*log(h[i]) + taum[i] + um[i]);
		res[i] = pow(h[i], 0.5)*z[i];
	}
}
