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
# include "stealfilters.h"
void real2garchfilter(int *model, double *pars, int *idx, double *res, double *z, double *vexdata, int T, int i, double *h, double *rsvp, double *rsvm,
		double *taup,double *taum, double *up,double *um)
{
	int j, ind;
	h[i] = h[i] +  pars[idx[6]];
	if( model[14]>0 )
	{
		for( j=0; j<model[14]; j++ )
		{
			ind = i + ( T * j );
			h[i] = h[i] + pars[idx[14]+j]*vexdata[ind];
		}
	}
	for( j=0; j<model[22]; j++ )
	{
		h[i] = h[i] + pars[idx[19]+j]*log(rsvp[i-(j+1)]);
	}
	for( j=0; j<model[23]; j++ )
	{
		h[i] = h[i] + pars[idx[20]+j]*log(rsvm[i-(j+1)]);
	}
	
	for( j=0; j<model[8]; j++ )
	{
		h[i] = h[i] + pars[idx[8]+j]*log(h[i-(j+1)]);
	}
	h[i] = exp(h[i]);
	z[i] = res[i]/sqrt(h[i]);
	taup[i] = pars[idx[25]]*z[i] +  pars[idx[26]]*(z[i]*z[i]-1);
	taum[i] = pars[idx[27]]*z[i] +  pars[idx[28]]*(z[i]*z[i]-1);
	up[i] = log(rsvp[i])-pars[idx[21]] - pars[idx[23]]*log(h[i]) - taup[i];
	um[i] = log(rsvm[i])-pars[idx[22]] - pars[idx[24]]*log(h[i]) - taum[i];
}










/*Tady je arfimaxfilter, protoze jsem se nechtel srat s  */ 