#' @export
stealugarchspec =  function(variance.model = list(
  model = "sGARCH",
  garchOrder = c(1, 1),
  submodel = NULL,
  external.regressors = NULL,
  variance.targeting = FALSE
),
mean.model = list(
  armaOrder = c(1, 1),
  include.mean = TRUE,
  archm = FALSE,
  archpow = 1,
  arfima = FALSE,
  external.regressors = NULL,
  archex = FALSE
),
distribution.model = "norm",
start.pars = list(),
fixed.pars = list(),
...)
{
  switch(variance.model$model,
         sGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         iGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         fiGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         eGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         gjrGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         apARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         fGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         csGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         mcsGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
         realGARCH = rugarch::ugarchspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...),
    ###### There starts the artificial addition

          real2GARCH = stealspec(variance.model = variance.model,mean.model = mean.model,distribution.model = distribution.model,start.pars = start.pars,fixed.pars = fixed.pars,... = ...)
  )

}

.expand.model = function(model) {
  modelnames = NULL
  for (i in 1:35) {
    if (i != 22) {
      if (model[i] > 0) {
        if (any(c(2, 3, 6, 8, 9, 10, 11, 12, 15, 23, 24, 29, 30, 31, 32) == i)) {
          modelnames = c(modelnames, paste(names(model)[i], 1:model[i], sep = ""))
        } else{
          modelnames = c(modelnames, names(model)[i])
        }
      }
    }
  }
  return(modelnames)
}

stealspec = function(variance.model = list(
  model = "real2GARCH",
  garchOrder = c(1, 1),
  submodel = NULL,
  external.regressors = NULL,
  variance.targeting = FALSE
),
mean.model = list(
  armaOrder = c(1, 1),
  include.mean = TRUE,
  archm = FALSE,
  archpow = 1,
  arfima = FALSE,
  external.regressors = NULL,
  archex = FALSE
),
distribution.model = "norm",
start.pars = list(),
fixed.pars = list(),
...) {
  UseMethod("stealspec")
}

.stealspec = function(variance.model = list(
  model = "real2GARCH",
  garchOrder = c(1, 1),
  submodel = NULL,
  external.regressors = NULL,
  variance.targeting = FALSE
),
mean.model = list(
  armaOrder = c(1, 1),
  include.mean = TRUE,
  archm = FALSE,
  archpow = 1,
  arfima = FALSE,
  external.regressors = NULL,
  archex = FALSE
),
distribution.model = "norm",
start.pars = list(),
fixed.pars = list(),
...)
{
  # some checks and preparation to be passed on to specific models by switch
  modelinc = rep(0, 35)
  # set the custom variance target to NA
  modelinc[22] = NA
  # [19] from aux to xi to accomodate realGARCH model (25-12-2014)
  names(modelinc) = c(
    "mu",
    "ar",
    "ma",
    "arfima",
    "archm",
    "mxreg",
    "omega",
    "alpha",
    "beta",
    "gamma",
    "eta1",
    "eta2",
    "delta",
    "lambda",
    "vxreg",
    "skew",
    "shape",
    "ghlambda",
    "xi",
    "aux",
    "aux",
    "aux",
    "alphap",
    "alpham",
    "xip",
    "xim",
    "deltap",
    "deltam",
    "eta1p",
    "eta2p",
    "eta1m",
    "eta2m",
    "lambdap",
    "lambdam",
    "lambdapm"
  )
  modeldesc = list()
  modeldata = list()

  # check the option parameters specified and stop on error
  mm = match(
    names(mean.model),
    c(
      "armaOrder",
      "include.mean",
      "archm",
      "archpow",
      "arfima",
      "external.regressors",
      "archex"
    )
  )
  if (any(is.na(mm))) {
    idx = which(is.na(mm))
    enx = NULL
    for (i in 1:length(idx))
      enx = c(enx, names(mean.model)[idx[i]])
    warning(paste(
      c("unidentified option(s) in mean.model:\n", enx),
      sep = "",
      collapse = " "
    ),
    call. = FALSE,
    domain = NULL)
  }
  vm = match(
    names(variance.model),
    c(
      "model",
      "garchOrder",
      "submodel",
      "external.regressors",
      "variance.targeting",
      "model.spec"
    )
  )
  if (any(is.na(vm))) {
    idx = which(is.na(vm))
    enx = NULL
    for (i in 1:length(idx))
      enx = c(enx, names(variance.model)[idx[i]])
    warning(paste(
      c("unidentified option(s) in variance.model:\n", enx),
      sep = "",
      collapse = " "
    ),
    call. = FALSE,
    domain = NULL)
  }

  # distribution model
  if (is.null(distribution.model))
    modeldesc$distribution = "norm"
  valid.distribution = c("norm",
                         "snorm",
                         "std",
                         "sstd",
                         "ged",
                         "sged",
                         "nig",
                         "ghyp",
                         "jsu",
                         "ghst")
  distribution = distribution.model

  if (!is.character(distribution[1]))
    stop("\nugarchspec-->error: cond.distribution argument must be a character")

  if (!any(distribution == valid.distribution))
    stop("\nugarchspec-->error: the cond.distribution does not appear to be a valid choice.")

  if (length(distribution) != 1)
    distribution = distribution[1]

  modeldesc$distribution = distribution
  modeldesc$distno = which(distribution == valid.distribution)
  di = .DistributionBounds(distribution)
  modelinc[16] = di$include.skew
  modelinc[17] = di$include.shape
  modelinc[18] = di$include.ghlambda
  # the last aux value is the distribution number
  modelinc[21] = modeldesc$distno
  # variance model:
  vmodel = list()

  valid.model = c(
    "sGARCH",
    "eGARCH",
    "gjrGARCH",
    "tGARCH",
    "fGARCH",
    "iGARCH",
    "fiGARCH",
    "apARCH",
    "csGARCH",
    "mcsGARCH",
    "realGARCH",
    "real2GARCH"
  )
  if (is.null(variance.model$model)) {
    modeldesc$vmodel = "sGARCH"
  } else{
    modeldesc$vmodel = variance.model$model[1]

    if (!is.character(modeldesc$vmodel))
      stop("\nugarchspec-->error: garch model argument must be a character.\n",
           call. = FALSE)

    if (!any(modeldesc$vmodel == valid.model))
      stop(
        "\nugarchspec-->error: the garch model does not appear to be a valid choice.\n",
        call. = FALSE
      )

    if (modeldesc$vmodel == "fGARCH") {
      modeldesc$vsubmodel = variance.model$submodel
      valid.submodel = c(
        "GARCH",
        "TGARCH",
        "AVGARCH",
        "NGARCH",
        "NAGARCH",
        "APARCH",
        "ALLGARCH",
        "GJRGARCH"
      )
      if (is.null(modeldesc$vsubmodel))
        stop(
          "\nugarchspec-->error: NULL not allowed for the submodel when model is of type fGARCH.\n",
          call. = FALSE
        )
      if (!any(modeldesc$vsubmodel == valid.submodel))
        stop(
          "\nugarchspec-->error: the fGARCH submodel does not appear to be a valid choice. See documentation for valid choices.\n",
          call. = FALSE
        )
    }
  }

  # depending on model include the additional parameters
  if (is.null(variance.model$garchOrder)) {
    modelinc[8] = 1
    modelinc[9] = 1
  } else{
    modelinc[8] = variance.model$garchOrder[1]
    modelinc[9] = variance.model$garchOrder[2]
  }

  if (modeldesc$vmodel == "gjrGARCH")
    modelinc[10] = modelinc[8]
  if (modeldesc$vmodel == "eGARCH")
    modelinc[10] = modelinc[8]
  if (modeldesc$vmodel == "fiGARCH") {
    # fractional parameter (FIGARCH): \delta
    # Hyperbolic Alpha Parameters (HYGARCH): \lambda
    modelinc[13] = 1
    # modelinc[14] = 1
  }

  if (modeldesc$vmodel == "apARCH") {
    modelinc[10] = modelinc[8]
    modelinc[13] = 1
  }
  if (modeldesc$vmodel == "fGARCH") {
    if (modeldesc$vsubmodel == "AVGARCH") {
      modelinc[12] = modelinc[11] = modelinc[8]
    }
    if (modeldesc$vsubmodel == "GJRGARCH")
      modelinc[11] = modelinc[8]
    if (modeldesc$vsubmodel == "TGARCH")
      modelinc[11] = modelinc[8]
    if (modeldesc$vsubmodel == "NGARCH")
      modelinc[14] = 1
    if (modeldesc$vsubmodel == "NAGARCH")
      modelinc[12] = modelinc[8]
    if (modeldesc$vsubmodel == "APARCH") {
      modelinc[14] = 1
      modelinc[11] = modelinc[8]
    }
    if (modeldesc$vsubmodel == "ALLGARCH") {
      modelinc[12] = modelinc[11] = modelinc[8]
      modelinc[14] = 1
    }
  }
  if (modeldesc$vmodel == "csGARCH") {
    modelinc[12] = modelinc[11] = 1
    variance.model$variance.targeting = vmodel$variance.targeting = FALSE
  }
  if (modeldesc$vmodel == "iGARCH" &&
      modelinc[9] == 0)
    stop("\nugarchspec-->error: the iGARCH model requires the GARCH beta parameter.\n",
         call. = FALSE)

  if (modeldesc$vmodel == "realGARCH") {
    # xi
    modelinc[19] = 1
    # \xi  + \delta \sigma _t^2 + {\eta _1}{z_t} + {\eta _2}\left( {z_t^2 - 1} \right) + {u_t}
    # lambda is now the variance of u_t
    modelinc[11:14] = 1
  }
  if (modeldesc$vmodel == "real2GARCH") {
    modelinc[23:24] = modelinc[8]
    modelinc[8] = 0
    modelinc[25:35] = 1
  }
  modeldata$vexdata = variance.model$external.regressors
  if (!is.null(variance.model$external.regressors))
    modelinc[15] = dim(variance.model$external.regressors)[2]

  if (is.null(variance.model$variance.targeting)) {
    modelinc[7] = 1
  } else{
    if (is.logical(variance.model$variance.targeting)) {
      modelinc[7] = as.integer(1 - variance.model$variance.targeting)
    } else{
      modelinc[7] = 0
      modelinc[22] = as.numeric(variance.model$variance.targeting)
    }
  }

  # mean model:
  if (is.null(mean.model$armaOrder)) {
    modelinc[2] = modelinc[3] = 1
  } else{
    modelinc[2] = mean.model$armaOrder[1]
    modelinc[3] = mean.model$armaOrder[2]
  }
  if (is.null(mean.model$include.mean))
    modelinc[1] = 1
  else
    modelinc[1] = as.integer(mean.model$include.mean)

  if (is.null(mean.model$archm) || !mean.model$archm) {
    modelinc[5] = 0
  } else{
    if (modeldesc$vmodel == "mcsGARCH")
      stop("\narchm not supported by mcsGARCH model.")
    if (is.null(mean.model$archpow))
      mean.model$archpow = 1
    modelinc[5] = as.integer(mean.model$archpow)
  }


  if (is.null(mean.model$arfima))
    modelinc[4] = 0
  else
    modelinc[4] = as.integer(mean.model$arfima)

  modeldata$mexdata = mean.model$external.regressors
  if (!is.null(mean.model$external.regressors))
    modelinc[6] = dim(mean.model$external.regressors)[2]

  if (is.null(mean.model$archex) || !mean.model$archex) {
    modelinc[20] = 0
  } else{
    if (modeldesc$vmodel == "mcsGARCH")
      stop("\narchex not supported by mcsGARCH model.")
    modelinc[20] = as.integer(mean.model$archex)
    if (modelinc[6] == 0)
      stop("\narchex cannot be used without external.regressors!!\n",
           call. = FALSE)
    if (modelinc[6] < modelinc[20])
      stop("\narchex cannot be greater than number of external.regressors!!\n",
           call. = FALSE)
  }

  maxOrder = max(modelinc[c(2, 3, 8, 9, 23, 24)])
  modelnames = .expand.model(modelinc)


  fmodel = NULL


  pos = 1
  pos.matrix = matrix(0, ncol = 3, nrow = 34)
  colnames(pos.matrix) = c("start", "stop", "include")
  rownames(pos.matrix) = c(
    "mu",
    "ar",
    "ma",
    "arfima",
    "archm",
    "mxreg",
    "omega",
    "alpha",
    "beta",
    "gamma",
    "eta1",
    "eta2",
    "delta",
    "lambda",
    "vxreg",
    "skew",
    "shape",
    "ghlambda",
    "xi",
    "aux",
    "aux",
    "alphap",
    "alphamm",
    "xip",
    "xim",
    "deltap",
    "deltam",
    "eta1p",
    "eta2p",
    "eta1m",
    "eta2m",
    "lambdap",
    "lambdam",
    "lambdapm"
  )

  # check if there are starting or fixed
  # check that it is included in the optimization
  # check that it is needed in the model


  if (modeldesc$vmodel == "fGARCH") {
    for (i in 1:21) {
      if (modelinc[i] > 0) {
        if (i == 11 && fmodel$finclude[3] == 1) {
          pos.matrix[i, 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:i, 2] + 1)
        } else if (i == 12 && fmodel$finclude[4] == 1) {
          pos.matrix[i, 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:i, 2] + 1)
        } else if (i == 13 && fmodel$finclude[2] == 1) {
          pos.matrix[i, 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:i, 2] + 1)
        } else if (i == 14 && fmodel$finclude[1] == 1) {
          pos.matrix[i, 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:i, 2] + 1)
        } else{
          pos.matrix[i, 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:i, 2] + 1)
        }
      }
    }
  } else{ ##### tady to chce special treatment pro real2garch
    for (i in 1:34) {
      if (i != 22) {
        if (i < 22) {
        if (modelinc[i] > 0) {
          pos.matrix[i, 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:i, 2] + 1)
        }
        } else {
          if (modelinc[i] > 0) {
          pos.matrix[(i-1), 1:3] = c(pos, pos + modelinc[i] - 1, 1)
          pos = max(pos.matrix[1:(i-1), 2] + 1)
          }
     }
    }
    }
  }
  ###c
  #cat(modelinc[24],modelinc[23],pos.matrix[20:23,1:3],sep = '\n',file = "pnames.txt",append = FALSE) #### modelinc[20] je 0
  nn = length(modelnames)
  modelmatrix = matrix(0, ncol = 3, nrow = nn)
  rownames(modelmatrix) = modelnames
  colnames(modelmatrix) = c("opt", "fixed", "start")
  fixed.names = names(fixed.pars)
  fp = charmatch(fixed.names, modelnames)

  if (!is.null(fixed.names) && any(!is.na(fp))) {
    fixed = fp[!is.na(fp)]
    modelmatrix[fixed, 2] = 1
    fz = charmatch(modelnames, fixed.names)
    fz = fz[!is.na(fz)]
    fixed.pars = fixed.pars[fz]
    names(fixed.pars) = fixed.names[fz]
  } else{
    fixed.pars = NULL
  }
  modelmatrix[, 1] = 1 - modelmatrix[, 2]
  start.names = names(start.pars)
  sp = charmatch(start.names, modelnames)
  if (!is.null(start.names) && any(!is.na(sp))) {
    start = sp[!is.na(sp)]
    modelmatrix[start, 3] = 1
    sz = charmatch(modelnames, start.names)
    sz = sz[!is.na(sz)]
    start.pars = start.pars[sz]
  } else{
    start.pars = NULL
  }


  ##################################################################
  # Parameter Matrix
  mm = sum(modelinc[c(2, 3, 6, 8, 9, 10, 11, 12, 15, 23, 24, 29, 30, 31, 32)])
  cat(mm,"prvni mm",sep = '\n',file = "pnames.txt",append = TRUE)
  mm = mm - length(which(modelinc[c(2, 3, 6, 8, 9, 10, 11, 12, 15, 23, 24, 29, 30, 31, 32)] >
                           0))
  cat(mm,"druhe mm",sep = '\n',file = "pnames.txt",append = TRUE)


  pars = matrix(0, ncol = 6, nrow = 32 + mm)
  colnames(pars) = c("Level", "Fixed", "Include", "Estimate", "LB", "UB")
  pidx = matrix(NA, nrow = 32, ncol = 2)
  colnames(pidx) = c("begin", "end")
  rownames(pidx) =  c(
    "mu",
    "ar",
    "ma",
    "arfima",
    "archm",
    "mxreg",
    "omega",
    "alpha",
    "beta",
    "gamma",
    "eta1",
    "eta2",
    "delta",
    "lambda",
    "vxreg",
    "skew",
    "shape",
    "ghlambda",
    "xi",
    "alphap",
    "alpham",
    "xip",
    "xim",
    "deltap",
    "deltam",
    "eta1p",
    "eta2p",
    "eta1m",
    "eta2m",
    "lambdap",
    "lambdam",
    "lambdapm"
  )
  fixed.names = names(fixed.pars)
  pnames = NULL
  nx = 0
  if (pos.matrix[1, 3] == 1) {
    pars[1, 3] = 1
    pars[1, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "mu"))
      pars[1, 2] = 1
    else
      pars[1, 4] = 1

  }
  pidx[1, 1] = 1
  pidx[1, 2] = 1
  pnames = c(pnames, "mu")
  nx = 1
  pn = 1
  pidx[2, 1] = 2
  if (pos.matrix[2, 3] == 1) {
    pn = length(seq(pos.matrix[2, 1], pos.matrix[2, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("ar", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0) {
        pars[(nx + i), 2] = 1}
      else{
        pars[(nx + i), 4] = 1}
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "ar")
  }
  pidx[2, 2] = 1 + pn

  nx = nx + pn
  pn = 1
  pidx[3, 1] = nx + 1
  if (pos.matrix[3, 3] == 1) {
    pn = length(seq(pos.matrix[3, 1], pos.matrix[3, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("ma", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0) {
        pars[(nx + i), 2] = 1
      }else{
        pars[(nx + i), 4] = 1 }
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "ma")
  }
  pidx[3, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[4, 1] = nx + 1
  if (pos.matrix[4, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 6) == "arfima"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pnames = c(pnames, "arfima")
  pidx[4, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[5, 1] = nx + 1
  if (pos.matrix[5, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 5) == "archm"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pnames = c(pnames, "archm")
  pidx[5, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[6, 1] = nx + 1
  if (pos.matrix[6, 3] == 1) {
    pn = length(seq(pos.matrix[6, 1], pos.matrix[6, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("mxreg", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "mxreg")
  }
  pidx[6, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[7, 1] = nx + 1
  if (pos.matrix[7, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 5) == "omega"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pnames = c(pnames, "omega")
  pidx[7, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[8, 1] = nx + 1
  if (pos.matrix[8, 3] == 1) {
    pn = length(seq(pos.matrix[8, 1], pos.matrix[8, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("alpha", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "alpha")
  }
  pidx[8, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[9, 1] = nx + 1
  if (pos.matrix[9, 3] == 1) {
    pn = length(seq(pos.matrix[9, 1], pos.matrix[9, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("beta", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)  pars[(nx + i), 2] = 1 else    pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
    #-------------------------------------------
    # special consideration for the iGARCH model
    #-------------------------------------------
    if (modeldesc$vmodel == "iGARCH") {
      # last beta not estimated
      pars[nx + pn, 4] = 0
      nnx = paste("beta", pn, sep = "")
      # do not allow the last beta to be fixed
      if (any(substr(fixed.names, 1, nchar(nnx)) == nnx))  pars[(nx + pn), 2] = 0
    }
  } else{
    pnames = c(pnames, "beta")
  }
  pidx[9, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[10, 1] = nx + 1

  if (pos.matrix[10, 3] == 1) {
    pn = length(seq(pos.matrix[10, 1], pos.matrix[10, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("gamma", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "gamma")
  }
  pidx[10, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[11, 1] = nx + 1

  if (pos.matrix[11, 3] == 1) {
    pn = length(seq(pos.matrix[11, 1], pos.matrix[11, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("eta1", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "eta1")
  }
  pidx[11, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[12, 1] = nx + 1

  if (pos.matrix[12, 3] == 1) {
    pn = length(seq(pos.matrix[12, 1], pos.matrix[12, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("eta2", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "eta2")
  }
  pidx[12, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[13, 1] = nx + 1

  if (pos.matrix[13, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 5) == "delta"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  } else{
    #-------------------------------------------
    # special consideration for the fGARCH model
    #-------------------------------------------
    if (modeldesc$vmodel == "fGARCH")
    {
      pars[nx + pn, 3] = 1
      pars[nx + pn, 1] = fmodel$fpars$delta
      pars[nx + pn, 4] = pars[nx + pn, 2] = 0
    }
  }
  pidx[13, 2] = nx + pn

  pnames = c(pnames, "delta")

  nx = nx + pn
  pn = 1
  pidx[14, 1] = nx + 1

  if (pos.matrix[14, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 6) == "lambda"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  } else{
    #-------------------------------------------
    # special consideration for the fGARCH model
    #-------------------------------------------
    if (modeldesc$vmodel == "fGARCH")
    {
      pars[nx + pn, 3] = 1
      pars[nx + pn, 1] = fmodel$fpars$lambda
      pars[nx + pn, 4] = pars[nx + pn, 2] = 0
    }
  }
  pidx[14, 2] = nx + pn

  pnames = c(pnames, "lambda")

  nx = nx + pn
  pn = 1
  pidx[15, 1] = nx + 1

  if (pos.matrix[15, 3] == 1) {
    pn = length(seq(pos.matrix[15, 1], pos.matrix[15, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("vxreg", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "vxreg")

  }
  pidx[15, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[16, 1] = nx + 1

  if (pos.matrix[16, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 4) == "skew"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[16, 2] = nx + pn

  pnames = c(pnames, "skew")

  nx = nx + pn
  pn = 1
  pidx[17, 1] = nx + 1

  if (pos.matrix[17, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 5) == "shape"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pnames = c(pnames, "shape")
  pidx[17, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[18, 1] = nx + 1

  if (pos.matrix[18, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 8) == "ghlambda"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[18, 2] = nx + pn
  pnames = c(pnames, "ghlambda")

  nx = nx + pn
  pn = 1
  pidx[19, 1] = nx + 1

  if (pos.matrix[19, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "xi"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[19, 2] = nx + pn
  pnames = c(pnames, "xi")


  ### Real2GARCH zone
  nx = nx + pn
  pn = 1
  pidx[20, 1] = nx + 1
  if (pos.matrix[22, 3] == 1) { ###g
    pn = length(seq(pos.matrix[22, 1], pos.matrix[22, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("alphap", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "alphap")

  }

  #cat(pos.matrix[20, 3],sep = '\n',file = "pnames.txt",append = TRUE) ### to co vychazi z te vstupni podminky
  ###c pos.matrix[20,3] je 0 i kdyz by melo byt 2
  pidx[20, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[21, 1] = nx + 1
  if (pos.matrix[23, 3] == 1) {
    pn = length(seq(pos.matrix[23, 1], pos.matrix[23, 2], by = 1))
    for (i in 1:pn) {
      pars[(nx + i), 1] = 0
      pars[(nx + i), 3] = 1
      nnx = paste("alpham", i, sep = "")
      sp = na.omit(match(fixed.names, nnx))
      if (length(sp) > 0)
        pars[(nx + i), 2] = 1
      else
        pars[(nx + i), 4] = 1
      pnames = c(pnames, nnx)
    }
  } else{
    pnames = c(pnames, "alpham")

  }

  pidx[21, 2] = nx + pn

  nx = nx + pn
  pn = 1
  pidx[22, 1] = nx + 1

  if (pos.matrix[24, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "xip"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[22, 2] = nx + pn
  pnames = c(pnames, "xip")

  nx = nx + pn
  pn = 1
  pidx[23, 1] = nx + 1

  if (pos.matrix[25, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "xim"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[23, 2] = nx + pn
  pnames = c(pnames, "xim")


  nx = nx + pn
  pn = 1
  pidx[24, 1] = nx + 1

  if (pos.matrix[26, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "deltap"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[24, 2] = nx + pn
  pnames = c(pnames, "deltap")

  nx = nx + pn
  pn = 1
  pidx[25, 1] = nx + 1

  if (pos.matrix[27, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "deltam"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[25, 2] = nx + pn
  pnames = c(pnames, "deltam")

  nx = nx + pn
  pn = 1
  pidx[26, 1] = nx + 1

  if (pos.matrix[28, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "eta1p"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[26, 2] = nx + pn
  pnames = c(pnames, "eta1p")

  nx = nx + pn
  pn = 1
  pidx[27, 1] = nx + 1

  if (pos.matrix[29, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "eta2p"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[27, 2] = nx + pn
  pnames = c(pnames, "eta2p")


  nx = nx + pn
  pn = 1
  pidx[28, 1] = nx + 1

  if (pos.matrix[30, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "eta1m"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[28, 2] = nx + pn
  pnames = c(pnames, "eta1m")


  nx = nx + pn
  pn = 1
  pidx[29, 1] = nx + 1

  if (pos.matrix[31, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "eta2m"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[29, 2] = nx + pn
  pnames = c(pnames, "eta2m")


  nx = nx + pn
  pn = 1
  pidx[30, 1] = nx + 1

  if (pos.matrix[32, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "lambdap"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[30, 2] = nx + pn
  pnames = c(pnames, "lambdap")

  nx = nx + pn
  pn = 1
  pidx[31, 1] = nx + 1

  if (pos.matrix[33, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "lambdam"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[31, 2] = nx + pn
  pnames = c(pnames, "lambdam")

  nx = nx + pn
  pn = 1
  pidx[32, 1] = nx + 1

  if (pos.matrix[34, 3] == 1) {
    pars[nx + pn, 3] = 1
    pars[nx + pn, 1] = 0
    if (any(substr(fixed.names, 1, 2) == "lambdapm"))
      pars[nx + pn, 2] = 1
    else
      pars[nx + pn, 4] = 1
  }
  pidx[32, 2] = nx + pn
  pnames = c(pnames, "lambdapm")


  #cat(pnames,length(pnames),rownames(pars),dim(pars),pos.matrix[34, 3],sep = '\n',file = "pnames.txt",append = TRUE) ### tady provadim debugging
  ###c problem u alphap - neprochazi ifem
  rownames(pars) = pnames

  zf = match(fixed.names, rownames(pars))
  if (length(zf) > 0)
    pars[zf, 1] = unlist(fixed.pars)
  pars[, "LB"] = NA
  pars[, "UB"] = NA


  model = list(
    modelinc = modelinc,
    modeldesc = modeldesc,
    modeldata = modeldata,
    pars = pars,
    start.pars = start.pars,
    fixed.pars = fixed.pars,
    maxOrder = maxOrder,
    pos.matrix = pos.matrix,
    fmodel = fmodel,
    pidx = pidx
  )
  ans = new("uGARCHspec", model = model)
  if (model$modeldesc$vmodel == "fiGARCH" &
      (model$modelinc["beta"] == 0 ||
       model$modelinc["alpha"] == 0))
    stop(paste0(
      "\nFIGARCH(",
      as.integer(model$modelinc['alpha']),
      ",d,",
      as.integer(model$modelinc['beta']),
      ") not a valid model"
    ))
  return(ans)
}


setMethod(f = "stealspec",definition = .stealspec) ## mozna je tokhle zbytecne

#' @export
getspec = function(object)
{
  UseMethod("getspec")
}
.getspec = function(object)
{
  spec = stealugarchspec(
    variance.model = list(
      model = object@model$modeldesc$vmodel,
      garchOrder = c(if (object@model$modeldesc$vmodel == 'real2GARCH') {
        object@model$modelinc[23]
      } else {

        object@model$modelinc[8]}

      ,
      object@model$modelinc[9]),
      submodel = object@model$modeldesc$vsubmodel,
      external.regressors = object@model$modeldata$vexdata
    ),
    mean.model = list(
      armaOrder = c(object@model$modelinc[2], object@model$modelinc[3]),
      include.mean = object@model$modelinc[1],
      archm = ifelse(object@model$modelinc[5]  >

                       0, TRUE, FALSE),
      archpow = object@model$modelinc[5],
      arfima = object@model$modelinc[4],
      external.regressors = object@model$modeldata$mexdata,
      archex = object@model$modelinc[
        20]
    ),
    distribution.model = object@model$modeldesc$distribution,
    start.pars  = object@model$start.pars,
    fixed.pars = object@model$fixed.pars
  )
  # should custom bounds be propagated?
  #idx = which(is.na(tmp@model$pars[,"LB"]))
  #tmp@model$pars[idx,"LB"] = object@model$pars[idx,"LB"]
  #idx = which(is.na(tmp@model$pars[,"UB"]))
  #tmp@model$pars[idx,"UB"] = object@model$pars[idx,"UB"]
  return(spec)
}

setMethod(f = "getspec", signature(object = "uGARCHfit"), definition = .getspec)


# internal function:
.model2spec = function(pars, model, type = "GARCH"){
  if(type == "GARCH"){
    ans = stealugarchspec(variance.model = list(model = model$modeldesc$vmodel, garchOrder = c(if(model$modeldesc$vmodel == 'real2GARCH') {modelinc[23]} else {modelinc[8]},model$modelinc[9]),
                                             submodel = model$modeldesc$vsubmodel, external.regressors = model$modeldata$vexdata),
                       mean.model = list(armaOrder = c(model$modelinc[2],model$modelinc[3]),
                                         include.mean = model$modelinc[1],
                                         archm = ifelse(model$modelinc[5]>0,TRUE,FALSE), archpow = model$modelinc[5],
                                         arfima = model$modelinc[4], external.regressors = model$modeldata$mexdata,
                                         archex = model$modelinc[20]),
                       distribution.model = model$modeldesc$distribution)
    setfixed(ans)<-pars
  } else{
    ans = arfimaspec(
      mean.model = list(armaOrder = c(model$modelinc[2],model$modelinc[3]),
                        include.mean = model$modelinc[1],
                        arfima = model$modelinc[4], external.regressors = model$modeldata$mexdata),
      distribution.model = model$modeldesc$distribution)
    setfixed(ans)<-pars
  }
  return(ans)
}

setGeneric("setfixed<-", function(object, value){standardGeneric("setfixed<-")})

.setfixed = function(object, value){
  # get parameter values
  model = object@model
  ipars = model$pars
  pars = unlist(value)
  names(pars) = parnames = tolower(names(pars))
  # included parameters in model
  modelnames = rownames(ipars[which(ipars[,4]==1 | ipars[,2]==1), ,drop=FALSE])
  inc = NULL
  for(i in seq_along(parnames)){
    if(is.na(match(parnames[i], modelnames))){
      warning( (paste("Unrecognized Parameter in Fixed Values: ", parnames[i], "...Ignored", sep = "")))
    } else{
      inc = c(inc, i)
    }
  }
  fixed.pars = pars[inc]
  names(fixed.pars) = tolower(names(pars[inc]))
  # check for variance.targeting
  if(!is.na(model$modelinc[22])){
    vt = model$modelinc[22]
  } else{
    vt = as.logical(1-model$modelinc[7])
  }
  # set parameter values
  tmp = stealugarchspec(variance.model = list(model = model$modeldesc$vmodel, garchOrder = c(if(model$modeldesc$vmodel == 'real2GARCH') {modelinc[23]} else {modelinc[8]}, model$modelinc[9]),
                                           submodel = model$modeldesc$vsubmodel, external.regressors = model$modeldata$vexdata,
                                           variance.targeting = vt),
                     mean.model = list(armaOrder = c(model$modelinc[2], model$modelinc[3]),
                                       include.mean = model$modelinc[1],
                                       archm = ifelse(model$modelinc[5]>0,TRUE,FALSE), archpow = model$modelinc[5],
                                       arfima = model$modelinc[4], external.regressors = model$modeldata$mexdata,
                                       archex = model$modelinc[20]),
                     distribution.model = model$modeldesc$distribution, start.pars  = model$start.pars,
                     fixed.pars = as.list(fixed.pars))
  # ToDo: Need to check that the parameters are not outside the bounds...
  idx = which(is.na(tmp@model$pars[,"LB"]))
  tmp@model$pars[idx,"LB"] = object@model$pars[idx,"LB"]
  idx = which(is.na(tmp@model$pars[,"UB"]))
  tmp@model$pars[idx,"UB"] = object@model$pars[idx,"UB"]
  return(tmp)
}
setReplaceMethod(f="setfixed", signature= c(object = "uGARCHspec", value = "vector"), definition = .setfixed)

setGeneric("setstart<-", function(object, value){standardGeneric("setstart<-")})

.setstart = function(object, value){
  # get parameter values
  model = object@model
  ipars = model$pars
  pars = unlist(value)
  names(pars) = parnames = tolower(names(pars))
  # included parameters in model
  modelnames = rownames(ipars[which(ipars[,4]==1 | ipars[,2]==1), , drop=FALSE])
  inc = NULL
  for(i in seq_along(parnames)){
    if(is.na(match(parnames[i], modelnames))){
      warning( (paste("Unrecognized Parameter in Start Values: ", parnames[i], "...Ignored", sep = "")))
    } else{
      inc = c(inc, i)
    }
  }
  start.pars = pars[inc]
  names(start.pars) = tolower(names(pars[inc]))
  # check for variance.targeting
  if(!is.na(model$modelinc[22])){
    vt = model$modelinc[22]
  } else{
    vt = as.logical(1-model$modelinc[7])
  }
  # set parameter values

  tmp = stealugarchspec(variance.model = list(model = model$modeldesc$vmodel, garchOrder = c(if(model$modeldesc$vmodel == 'real2GARCH') {modelinc[23]} else {modelinc[8]}, model$modelinc[9]),
                                           submodel = model$modeldesc$vsubmodel, external.regressors = model$modeldata$vexdata,
                                           variance.targeting = vt),
                     mean.model = list(armaOrder = c(model$modelinc[2], model$modelinc[3]),
                                       include.mean = model$modelinc[1],
                                       archm = ifelse(model$modelinc[5]>0,TRUE,FALSE), archpow = model$modelinc[5],
                                       arfima = model$modelinc[4], external.regressors = model$modeldata$mexdata,
                                       archex = model$modelinc[20]),
                     distribution.model = model$modeldesc$distribution, fixed.pars  = model$fixed.pars,
                     start.pars = as.list(start.pars))
  # ToDo: Need to check that the parameters are not outside the bounds...
  idx = which(is.na(tmp@model$pars[,"LB"]))
  tmp@model$pars[idx,"LB"] = object@model$pars[idx,"LB"]
  idx = which(is.na(tmp@model$pars[,"UB"]))
  tmp@model$pars[idx,"UB"] = object@model$pars[idx,"UB"]
  return(tmp)
}
#----------------------------------------------------------------------------------
# univariate model dispatch methods
#----------------------------------------------------------------------------------


.stealugarchfit = function(spec, data, out.sample = 0, solver = "solnp", solver.control = list(),
                        fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all', trunclag = 1000),
                        numderiv.control = list(grad.eps=1e-4, grad.d=0.0001, grad.zero.tol=sqrt(.Machine$double.eps/7e-7),
                                                hess.eps=1e-4, hess.d=0.1, hess.zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2),...)
{
  default.numd = list(grad.eps=1e-4, grad.d=0.0001, grad.zero.tol=sqrt(.Machine$double.eps/7e-7),
                      hess.eps=1e-4, hess.d=0.1, hess.zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2)
  idx1 = na.omit(match(names(numderiv.control), names(default.numd)))
  idx2 = na.omit(match(names(default.numd), names(numderiv.control)))
  if(length(idx1)>0){
    default.numd[idx1] = numderiv.control[idx2]
  }

  return(if(spec@model$modeldesc$vmodel %in% c(
    "sGARCH",
    "eGARCH",
    "gjrGARCH",
    "tGARCH",
    "fGARCH",
    "iGARCH",
    "fiGARCH",
    "apARCH",
    "csGARCH",
    "mcsGARCH",
    "realGARCH")) { rugarch::ugarchfit(spec = spec,
                                     data = data,
                                     out.sample = out.sample,
                                     solver = solver,
                                     solver.control = solver.control,
                                     fit.control = fit.control,
                                     numderiv.control = numderiv.control,
                                     ... = ...) } else {

    switch(spec@model$modeldesc$vmodel,
                 real2GARCH = .real2garchfit(spec = spec, data = data, out.sample = out.sample, solver = solver,
                                             solver.control = solver.control, fit.control = fit.control, numderiv.control = default.numd, ...)
  )})
}

setReplaceMethod(f="setstart", signature= c(object = "uGARCHspec", value = "vector"), definition = .setstart)

.checkallfixed = function( spec ){
  # check that a given spec with fixed parameters
  model = spec@model
  pars = model$pars
  pnames = rownames(pars)
  estpars = pnames[as.logical(pars[,2] * pars[,3] + pars[,3] * pars[,4])]
  return( estpars )

}

setGeneric("setbounds<-", function(object, value){standardGeneric("setbounds<-")})


.setbounds = function(object, value){
  model = object@model
  ipars = model$pars
  parnames = tolower(names(value))
  # included parameters in model
  modelnames = rownames(ipars[which(ipars[,4] == 1), ])
  sp = na.omit(match(parnames, modelnames))
  if(length(sp)>0){
    for(i in 1:length(sp)){
      #if(length(value[[modelnames[sp[i]]]])!=2)
      ipars[modelnames[sp[i]], 5] = as.numeric(value[[modelnames[sp[i]]]][1])
      ipars[modelnames[sp[i]], 6] = as.numeric(value[[modelnames[sp[i]]]][2])
    }
  }
  object@model$pars = ipars
  return(object)
}
setReplaceMethod(f="setbounds", signature= c(object = "uGARCHspec", value = "vector"), definition = .setbounds)

.stealugarchfilter = function(spec, data, out.sample = 0, n.old = NULL, rec.init = 'all', trunclag = 1000, ...)
{
  return(if(spec@model$modeldesc$vmodel %in% c(
      "sGARCH",
      "eGARCH",
      "gjrGARCH",
      "tGARCH",
      "fGARCH",
      "iGARCH",
      "fiGARCH",
      "apARCH",
      "csGARCH",
      "mcsGARCH",
      "realGARCH")) {
    rugarch::ugarchfilter(spec = spec,
                          data = data,
                          out.sample = out.sample,
                          n.old = n.old, rec.init = rec.init,... = ...)
    } else {
      switch(spec@model$modeldesc$vmodel,
             real2GARCH = .real2garchfilter(spec = spec,
                                            data = data,
                                            out.sample = out.sample,
                                            n.old = n.old,
                                            rec.init = rec.init,
                                            trunclag = trunclag,
                                            ... = ...)
                                           )})

}




.ugarchforecast1 = function(fitORspec, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0,
                            external.forecasts = list(mregfor = NULL, vregfor = NULL), ...)
{
  return( switch(fitORspec@model$modeldesc$vmodel,
                 sGARCH = .sgarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                          out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 iGARCH = .igarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                          out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 fiGARCH = .figarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                            out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 eGARCH = .egarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                          out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 gjrGARCH = .gjrgarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                              out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 apARCH = .aparchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                          out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 fGARCH = .fgarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                          out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 csGARCH = .csgarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                            out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 mcsGARCH = .mcsgarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                              out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 realGARCH = .realgarchforecast(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                                out.sample = out.sample, external.forecasts = external.forecasts, ...)) )
}

.ugarchforecast2 = function(fitORspec, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0,
                            external.forecasts = list(mregfor = NULL, vregfor = NULL), trunclag = 1000, ...)
{
  return( switch(fitORspec@model$modeldesc$vmodel,
                 sGARCH = .sgarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                           out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 iGARCH = .igarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                           out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 fiGARCH = .figarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                             out.sample = out.sample, external.forecasts = external.forecasts, trunclag = trunclag, ...),
                 eGARCH = .egarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                           out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 gjrGARCH = .gjrgarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                               out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 apARCH = .aparchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                           out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 fGARCH = .fgarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                           out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 csGARCH = .csgarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                             out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 mcsGARCH = .mcsgarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                               out.sample = out.sample, external.forecasts = external.forecasts, ...),
                 realGARCH = .realgarchforecast2(fitORspec = fitORspec, data = data, n.ahead = n.ahead, n.roll = n.roll,
                                                 out.sample = out.sample, external.forecasts = external.forecasts, ...)) )
}

.ugarchsim = function(fit, n.sim = 1000, n.start = 0, m.sim = 1, startMethod = c("unconditional","sample"),
                      presigma = NA, prereturns = NA, preresiduals = NA, rseed = NA,  custom.dist = list(name = NA, distfit = NA),
                      mexsimdata = NULL, vexsimdata = NULL, ...)
{
  return( switch(fit@model$modeldesc$vmodel,
                 sGARCH = .sgarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                     startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                     preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                     mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 iGARCH = .igarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                     startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                     preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                     mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 fiGARCH = .figarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                       startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                       preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                       mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 eGARCH = .egarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                     startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                     preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                     mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 gjrGARCH = .gjrgarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                         startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                         preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                         mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 apARCH = .aparchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                     startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                     preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                     mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 fGARCH = .fgarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                     startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                     preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                     mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 csGARCH = .csgarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                       startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                       preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                       mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 mcsGARCH = .mcsgarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                         startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                         preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                         mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...),
                 realGARCH = .realgarchsim(fit = fit, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                           startMethod = startMethod, presigma = presigma, prereturns = prereturns,
                                           preresiduals = preresiduals, rseed = rseed,  custom.dist = custom.dist,
                                           mexsimdata = mexsimdata, vexsimdata = vexsimdata, ...)) )
}

.ugarchpath = function(spec, n.sim = 1000, n.start = 0, m.sim = 1, presigma = NA, prereturns = NA, preresiduals = NA,
                       rseed = NA, custom.dist = list(name = NA, distfit = NA), mexsimdata = NULL,  vexsimdata = NULL,
                       trunclag = 1000, ...)
{
  return( switch(spec@model$modeldesc$vmodel,
                 sGARCH = .sgarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                      presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                      rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                      vexsimdata = vexsimdata, ...),
                 iGARCH = .igarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                      presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                      rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                      vexsimdata = vexsimdata, ...),
                 fiGARCH = .figarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                        presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                        rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                        vexsimdata = vexsimdata, trunclag = trunclag, ...),
                 eGARCH = .egarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                      presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                      rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                      vexsimdata = vexsimdata, ...),
                 gjrGARCH = .gjrgarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                          presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                          rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                          vexsimdata = vexsimdata, ...),
                 apARCH = .aparchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                      presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                      rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                      vexsimdata = vexsimdata, ...),
                 fGARCH = .fgarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                      presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                      rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                      vexsimdata = vexsimdata, ...),
                 csGARCH = .csgarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                        presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                        rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                        vexsimdata = vexsimdata, ...),
                 realGARCH = .realgarchpath(spec = spec, n.sim = n.sim, n.start = n.start, m.sim = m.sim,
                                            presigma = presigma, prereturns = prereturns, preresiduals = preresiduals,
                                            rseed = rseed,  custom.dist = custom.dist, mexsimdata = mexsimdata,
                                            vexsimdata = vexsimdata, ...)) )
}

#----------------------------------------------------------------------------------
# univariate filter method
#----------------------------------------------------------------------------------
stealugarchfilter = function(spec, data, out.sample = 0, n.old = NULL, rec.init = 'all', trunclag = 1000, ...)
{
  UseMethod("stealugarchfilter")
}

setMethod("stealugarchfilter", signature(spec = "uGARCHspec"), .stealugarchfilter)

#----------------------------------------------------------------------------------
# univariate fit method
#----------------------------------------------------------------------------------
stealugarchfit = function(spec, data, out.sample = 0, solver = "solnp", solver.control = list(),
                       fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all', trunclag = 1000),
                       numderiv.control = list(grad.eps=1e-4, grad.d=0.0001, grad.zero.tol=sqrt(.Machine$double.eps/7e-7),
                                               hess.eps=1e-4, hess.d=0.1, hess.zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2), ...)
{
  UseMethod("stelaugarchfit")
}

setMethod("stealugarchfit", signature(spec = "uGARCHspec"), .stealugarchfit)




#----------------------------------------------------------------------------------
# persistence
#----------------------------------------------------------------------------------

.persistreal2garch1 = function(pars, idx, distribution = "norm"){
  alphap = pars[idx["alphap",1]:idx["alphap",2]]
  alpham = pars[idx["alpham",1]:idx["alpham",2]]
  beta  = pars[idx["beta",1]:idx["beta",2]]
  deltap = pars[idx["deltap",1]:idx["deltap",2]]
  deltam = pars[idx["deltam",1]:idx["deltam",2]]
  ps = sum(beta) + deltap*sum(alphap)+ deltam*sum(alpham)
  return(ps)
}

.persistreal2garch2 = function(pars, distribution = "norm"){
  Names = names(pars)
  if(any(substr(Names, 1, 6)=="alphap")){
    i = which(substr(Names, 1, 6)=="alphap")
    alphap = pars[i]
  } else{
    alphap = 0
  }
  if(any(substr(Names, 1, 6)=="alpham")){
    i = which(substr(Names, 1, 6)=="alpham")
    alpham = pars[i]
  } else{
    alpham = 0
  }

  if(any(substr(Names, 1, 6)=="deltap")){
    i = which(substr(Names, 1, 6)=="deltap")
    deltap = pars[i]
  } else{
    deltap = 0
  }
  if(any(substr(Names, 1, 6)=="deltam")){
    i = which(substr(Names, 1, 6)=="deltam")
    deltam = pars[i]
  } else{
    deltam = 0
  }

  if(any(substr(Names, 1, 4)=="beta")){
    i = which(substr(Names, 1, 4)=="beta")
    beta = pars[i]
  } else{
    beta = 0
  }
  ps = sum(beta) + deltap*sum(alphap)+ deltam*sum(alpham)
  return(ps)
}
