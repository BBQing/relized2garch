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
#ifndef STEALFILTERS_H
#define STEALFILTERS_H
void real2garchfilter(int *model, double *pars, int *idx, double *res, double *z, double *vexdata, int T, int i, double *h, double *rsvp, double *rsvm,
		double *taup,double *taum, double *up,double *um);
		
		
#endif /* STEALFILTERS_H */