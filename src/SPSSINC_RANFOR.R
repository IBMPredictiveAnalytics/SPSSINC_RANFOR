#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2022
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

#__author__ = "SPSS, JKP"
#__version__ = "1.2.0"

# History
# 30-Sep-2008 Original version
# 12-Nov-2008  Improve line wrapping strategy for very long variable lists.
# 10-Apr-2013 Rewrite to simplify and eliminate need for Python plugin
# 24-May-2013 Tinker with display parameters in var imp plot to deal with long names
# 26-nOV-2022 Add support for a case id variable in output files and add dep var to outliers dataset.

# helptext is no longer maintained in favor the syntax help file

helptext="SPSSINC RANFOR DEPENDENT=dependent variable ENTER=variable list
UNSUPERVISED = {NO* | YES}
[/OPTIONS [MISSING={RFIMPUTE*|ROUGH|FAIL}
[NUMTREES=integer] [VARSSAMPLED=integer]
[CLASSPRIORS=list of values]
[MINNODESIZE=integer]
[RANDOMNUMBERSEED]]

[SAVE [IMPUTEDDATASET=datasetname]
[PREDVALUES=datasetname] [OUTLIERS=datasetname]
[FOREST=R filespec]
[RETAINFOREST]]

[PRINT  [VARUSAGE]]

[PLOT [PARTIALPLOTS=selectedpredictors or ALL] [VARIABLEIMPORTANCE]


Split files and weight are not honored by this command.

SPSSINC RANFOR /HELP.  prints this information and does nothing else.

Example:
SPSSINC RANFOR DEPENDENT=mpg ENTER=engine weight.

Execute the R randomForest procedure.
'Random Forest' is a trademark of Breiman and Cutler.

DEPENDENT and ENTER specify the dependent and independent
variable names.

Categorical  variables are automatically converted
appropriately to factors.  If the dependent variable is categorical,
classification is performed; otherwise regression is used.

UNSUPERVISED = YES can be used to analyze proximities among the cases.
The dependent variable can be omitted in this case.  However,
if a dependent variable is used, the output automatically includes 
the MDS plot.

MISSING = RFIMPUTE causes a forest to be used to impute missing values
in predictors.  The dependent variable may not have missing values.
RFIMPUTE may not be used if UNSUPERVISED=YES
MISSING = ROUGH causes missing values to be imputed for all variables.  
Scale variables are imputed as the variable median, and categorical variables 
as the mode with ties broken at random.
MISSING = FAIL stops the procedure if missing values are encountered.

NUMTREES specifies the number of trees to use and defaults to 500.
VARSSAMPLED specifies the number of variables to consider at each 
node split.  It defaults to the number of predictors/3 for a scale dependent 
variable and sqrt(number of predictors) for a categorical variable.

CLASSPRIORS applies only to classification and specifies priors for each
dependent variable category listed in ascending category order.  
The values need not sum to 1.

MINNODESIZE specifies the minimum size of terminal nodes in the tree.  
It defaults to 5 for regression and 1 for classification.

RANDOMNUMBERSEED sets the random number seed for
reproducibility from run to run.  This affects only the R random number
generator.

/SAVE FOREST saves the actual R randomForest structure in an R workspace in the 
specified file (typically with extension Rdata).  This option or RETAINFOREST should 
be used if predictions are to be made with SPSSINC RANFOR PREDICT.  Predictions
cannot be made if UNSUPERVISED=YES

By default, the forest data structure is deleted from memory after results are produced.
Use RETAINFOREST to keep the data structure in memory.  This is useful if
proceeding to prediction with new data in the same session.  The data structure
will be replaced if this command is rerun.

IMPUTEDDATASET names a dataset to be created with the data values after imputation.
If no imputation is done but an imputed dataset name is specified, the data values 
will be the same as the original data apart from any filter in effect.  Value labels and 
missing value definitions are not applied but can be brought over using APPLY DICTIONARY.

PREDVALUES names a dataset to be created with the predicted values.  It will contain 
only the cases sent to the analysis after any filter is applied.
This is not available if UNSUPERVISED=YES

OUTLIERS names a dataset to be created with an outlier measure for each case in the 
analysis.  It is only available for classification analysis.  The measure is
n / sum(squared proximity), normalized by subtracting the median and divided by 
the MAD, within each class.  The proximity matrix is of order n**2 where n is the number
of cases, so use this option with caution.  If a dependent variable is
not specified, which requires UNSUPERVISED=YES, all cases are considered
to be in the same class.  If a dependent variable is specified, even if
UNSUPERVISED=YES, it defines classes used for calculating this measure.

The default Viewer output includes a summary table, the Variable Importance table, 
and a plot of error rates versus the number of trees.  Additional output available is
VARUSAGE, which shows the frequency of usage of each predictor variable in 
the forest.  The error rate plot is not available if UNSUPERVISED=YES.

VARIABLEIMPORTANCE, which plots the variable importance
PARTIALPLOTS, showing the marginal effect of the specified variables on the 
response or the class probability.
It can be ALL to plot all predictors or a list of predictors.
Not available if UNSUPERVISED=YES.

This extension command requires the  R programmability plug-ins and 
the R randomForest package.
"

###options(error=traceback)

ranfor = function(dep=NULL, indep, missing="rough",  numtrees=500, varssampled=NULL, minnodesize=NULL,
    classpriors=NULL, unsupervised=FALSE,
    predvalues=NULL, imputeddataset=NULL, forest=NULL,
    importance=TRUE, varusage=FALSE, partialplots=NULL, seed=NULL, 
    mdsplot=FALSE, mdsplotdim=2, programfile=NULL,
    variableimportance=FALSE, outlierds=NULL, errorrates=TRUE, retainforest=FALSE,
    caseid=NULL) {
    
    setuplocalization()
    if (!is.null(spssdictionary.GetWeightVariable())) {
      stop(gtxt("The SPSSINC RANFOR procedure does not support weights"))
    }
    if (!is.null(spssdata.GetSplitVariableNames())) {
      print(gtxt("Warning: SPLIT FILES is not supported by SPSSINC RANFOR.  Splits will be ignored."))
    }

    tryCatch(library(randomForest), error=function(e){
        stop(gtxtf("The R %s package is required but could not be loaded.","randomForest"),call.=FALSE)
        }
    )
    if (!is.null(programfile)) {
        print(gtxt("Warning: The programfile keyword is no longer supported"))
        programfile = NULL
    }
    if (is.null(dep) && !unsupervised) {
        stop(gtxt("A dependent variable must be specified except for the unsupervised case"), call.=FALSE)
    }
    
    allvars = c(dep, indep, caseid)  # if dep or caseid is null (unsupervised), it disappears from allvars
    model = paste(indep, collapse="+")
    if (!unsupervised) {
        model = paste(dep, model, sep="~")
    }
    if (length(union(indep,indep)) != length(indep)) {
        stop(gtxt("The same predictor variable was entered more than once"), call.=FALSE)
    }
    # test for dataset names in use    
    alldsspecs = c(predvalues, imputeddataset, outlierds)
    if (!is.null(alldsspecs)) {
        alldatasets = spssdata.GetDataSetList()
        if (length(intersect(alldsspecs, alldatasets) > 0)) {
            stop(gtxt("One or more specified output dataset names are already in use"), call.=FALSE)
        }
    }

    dta <- spssdata.GetDataFromSPSS(allvars, missingValueToNA = TRUE, 
    keepUserMissing=FALSE, factorMode = "levels")

    if (unsupervised) {
        classify = TRUE
        classification = gtxt("unsupervised")
        if (!is.null(dep) && !is.factor(dta[[dep]])) {
            stop(gtxt("Unsupervised mode cannot be used with a scale dependent variable"))
        }
    } else {
        classify = is.factor(dta[[dep]])
        classification = ifelse(classify, gtxt("classification"), gtxt("regression"))
        if (classify && !is.null(classpriors) && length(levels(dta[[dep]])) != length(classpriors)) {
            stop(gtxt("The number of class priors specified differs from the number of levels of the dependent variable"),
                call.=FALSE)
        }
    }
    if (!is.null(caseid)) {
      caseiddta = dta[ncol(dta)]
      dta = dta[-ncol(dta)]
    } else {
      caseiddta = NULL
      
    }
    if (!classify && !is.null(classpriors)) {
        stop(gtxt("Class priors were specified, but the dependent variable is not categorical"), call.=FALSE)
    }
    
    classpriorstr = ifelse(is.null(classpriors), "NA", paste(classpriors, collapse=" "))
    if (!classify && mdsplot) {
        mdsplot = FALSE
        print(gtxt("The MDS plot is only available for classification trees"))
    }

    if (is.null(minnodesize)) {
        if (is.null(classify)) {
            minnodesize=5
        } else if (classify) {
            minnodesize = 1
        } else {
            minnodesize = 5
        }
    }
    if (is.null(seed)) {
        seed = gtxt("Not Set")
    } else {
        set.seed(seed)
    }
    if (missing == "rfimpute" && unsupervised) {
        stop(gtxt("RF imputation is not available for the unsupervised case"), call.=FALSE)
    }
    if (missing == "rfimpute") {
      # impute predictors - will complain if no missing values
      # rfImpute returns the whole data frame, not just the predictors!
      # Not implemented for unsupervised case
      dta= data.frame(dta[1], tryCatch(
        rfImpute(dta[,-1], y=dta[,1], minnodesize=minnodesize), error=function(e) {print(e); return(dta)})[-1])
    } else if (missing == "rough") {
        dta = na.roughfix(dta)
    }

    if (is.null(varssampled)) {
        if (is.null(classify) || classify) {
            varssampled = as.integer(sqrt(length(indep)))
        } else {
            varssampled = as.integer(max(length(indep)/3, 1))
        }
    }
    spssdict <- spssdictionary.GetDictionaryFromSPSS(allvars)

    prox = ifelse(is.null(outlierds) && !mdsplot, FALSE, TRUE)

    if (unsupervised) {res  <- tryCatch(randomForest(data.frame(dta[,-1]), ntree=numtrees, mtry=varssampled,
        nodesize=minnodesize, classwt = classpriors, proximity=prox), error=function(e) stop(as.character(e), call.=FALSE))
    } else {
        res  <- tryCatch(randomForest(data.frame(dta[,-1]), y=dta[,1], ntree=numtrees, mtry=varssampled,
        nodesize=minnodesize, classwt = classpriors, proximity=prox), error=function(e) stop(as.character(e), call.=FALSE))
    }

    StartProcedure("Ranfor", "SPSSINCRANFOR")
    if (errorrates) {
        if (unsupervised) {
            print(gtxt("The error rate plot is not available in unsupervised mode."))
        } else {
            plot(res, main=gtxt("Error Rates"), lwd=2)
        }
    }

    if (variableimportance) {
        varimpplot = varImpPlot(res, sort=TRUE, main=gtxt("Variable Importance Plot"), pch=19)
    }
    if (mdsplot) {
        if (is.null(dep)) {
            print(gtxt("The MDS plot is not available unless a dependent variable is specified"))
        } else {
            # distinguish points by shape as well as color as long
            # as there are not more than 4 levels
            if (nlevels(dta[[1]]) <= 4) {
                pch = c(19, 17, 15, 18)
                pp = pch[as.numeric(dta[[1]])]
            } else {
                pp = 19
            }
            MDSplot(res, fac=dta[[1]], k=mdsplotdim, main=gtxt("Multi-Dimensional Scaling Plot of Proximities"), 
            pch=pp, cex=1.5)
        }
    }


    varcount = length(dta) - 1

    if (!unsupervised) {
        tsstats=summary(treesize(res))[c(2,3,5)]
        tsstatslbls = c(gtxt("Tree Size Terminal Nodes: 1st Quartile"),gtxt("Tree Size Terminal Nodes: Median"),
            gtxt("Tree Size Terminal Nodes: 3rd Quartile"))
    } else {
        tsstats = NULL
        tsstatslbls = NULL
    }
    sumlbls = c(gtxt("Tree Type"), gtxt("Dependent Variable"), gtxt("Predictors"),
        gtxt("Trees"), gtxt("Variable Tries Per Split"), gtxt("Predictor Imputation"))

    # Summary table

    if (res$type == "regression") {
        resids = dta[1]-res$predicted
        tbl1lbls=c(sumlbls, gtxt("Residual Mean Square"),
            gtxt("Explained Variance Percentage"), 
            tsstatslbls,
            gtxt("Random Number Seed"), 
            gtxt("Forest Workspace"), 
            gtxt("Workspace retained in memory"))
        tbl1values=c(res$type, 
            dep, 
            paste(dimnames(dta)[[2]][-1], collapse=" "),
            res$ntree, 
            res$mtry, 
            missing, 
            sum(resids*resids)/length(res$predicted),
            1 - var(resids)/var(dta[1]), 
            tsstats, 
            seed, 
            ifelse(!is.null(forest),forest,gtxt("--Not saved--")), 
            ifelse(retainforest, gtxt("Yes"), gtxt("No"))
        )
        } else { # classification or unsupervised
        tbl1lbls=c(sumlbls, 
            gtxt("Class Priors"), 
            gtxt("Out of Bag Estimated Error Rate"), 
            tsstatslbls,
            gtxt("Random Number Seed"), 
            gtxt("Forest Workspace"), 
            gtxt("Workspace retained in memory"),
            gtxt("Case ID variable"),
            gtxt("Imputed dataset"),
            gtxt("Predicted values dataset"),
            gtxt("Outliers dataset")
            
        )
        tbl1values=c(res$type, 
            ifelse(is.null(dep), gtxt("None specified"), dep), 
            paste(dimnames(dta)[[2]][-1], collapse=" "),
            res$ntree, 
            res$mtry, 
            missing, 
            classpriorstr,
            ifelse(unsupervised, NA, res$err.rate[[res$ntree]]), 
            tsstats, 
            seed, 
            ifelse(!is.null(forest), forest,gtxt("Not saved")), 
            ifelse(retainforest, gtxt("Yes"), gtxt("No")),
            ifelse(is.null(caseid), gtxt("-None-"), caseid),
            ifelse(is.null(imputeddataset) || missing == "fail", gtxt("-None-"), imputeddataset),
            ifelse(is.null(predvalues) || unsupervised, gtxt("-None-"), predvalues),
            ifelse(is.null(outlierds) || !classify, gtxt("-None-"), outlierds)
        )
    }

    spsspivottable.Display(tbl1values, gtxt("Random Forest Summary"), "RANFORSUMMARY",
        caption=gtxtf("Random Forest computed by R randomForest package %s", packageVersion("randomForest")),
        isSplit=FALSE,
        rowlabels = tbl1lbls,
        collabels=gtxt("Statistics"))
        
    if (varcount == 1) {
        attr(res$importance, which="dimnames")[[1]] = dimnames(dta)[[2]][[2]]
        if (!is.null(res$forest$ncat)) {
            attr(res$forest$ncat,which="names") = dimnames(dta)[[2]][[2]]
        }
    }

    if (importance) {
        if (is.null(classify) || classify) {
            importancecaption = gtxt("Total decrease in node impurities from splitting on the variable averaged over all trees measured by the Gini index")
        } else {
            importancecaption = gtxt("Total decrease in node impurities from splitting on the variable averaged over all trees measured by the residual sum of squares")
        }
        imp = importance(res)
        imp = imp[order(imp, decreasing=TRUE),]
        spsspivottable.Display(imp, gtxt("Variable Importance"), "RANFORIMPORTANCE",
        caption=importancecaption, 
        isSplit=FALSE,
        rowlabels= row.names(imp), collabels=gtxt("Decrease in Node Impurity"))
    }

    if (varusage) {
        if (unsupervised) {
            print(gtxt("Variable usage information is not available in unsupervised mode"))
        } else {
            vu = varUsed(res)   # listed in order in dta
            spsspivottable.Display(vu, gtxt("Predictor Variable Usage"), "RANFORVARUSAGE",
            caption=gtxt("Frequencies of predictor variable usage in the forest"),
            isSplit=FALSE,
            rowlabels = dimnames(dta)[[2]][-1],
            collabels=gtxt("Frequency"), format=formatSpec.Count)
        }
    }

    if (res$type == "classification") {
        confus = res$confusion
        # add row totals.  omit last column from total as it is the class error
        rowtot = apply(confus[,-ncol(confus)], 1, sum)
        meanerror = confus[,ncol(confus)] %*% rowtot / sum(rowtot)
        confus = cbind(confus, rowtot)
        coltot = apply(confus, 2, sum)
        coltot[length(coltot)-1] = meanerror
        confus = rbind(confus, coltot)
        row.names(confus)[[nrow(confus)]] = gtxt("Column Total")
        spsspivottable.Display(confus, outline=gtxt("Confusion Matrix of Predictions"), 
            templateName="RANFORCONFUSION", title=gtxt("Confusion Matrix of Predictions"),
            isSplit=FALSE, 
            caption=gtxt("Rows are actuals; columns are predicted.  Last Class Error is the overall error rate."),
            rowlabels = row.names(confus), rowdim=dep, hiderowdimtitle=FALSE,
            coldim=gtxt("Predicted"), hidecoldimtitle=FALSE,
            collabels=c(row.names(confus)[-nrow(confus)], gtxt("Class Error"), gtxt("Row Total")))
    }
    if (!is.null(partialplots) && tolower(partialplots[[1]]) == "all") {
        partialplots = indep
    }

    # doing the obvious in the partialPlot call does not work, because that function
    # is doing a substitute on v, producing v instead of the value of v and thereby
    # can't find the actual variable being plotted.  :-) :-) :-)!
    if (!unsupervised) {
        for (v in partialplots) {
            if (v %in% indep) {
                do.call(partialPlot, list(res, pred.data=dta, x.var=v, lwd=2,
                    main=gtxtf("Partial Dependence: %s", v), xlab=v))
            }
        }
    }
    if (!is.null(forest)) {
        forest = gsub(pattern="\\",replacement="/", x=forest, fixed=TRUE)
        forestfile=save(res, tbl1lbls, tbl1values, spssdict, file=forest)
    }
    spsspkg.EndProcedure()
    # save requested datasets
    if (!is.null(imputeddataset)) {
        saveimputeddataset(imputeddataset, spssdict, dta, caseid, caseiddta)
    }
    if (!is.null(predvalues)) {
        savepreddataset(predvalues, spssdict, res, dep, caseid, caseiddta)
    }
    if (!is.null(outlierds) && classify) {
        saveoutliersdataset(outlierds, spssdict, res, dep, dta, caseid, caseiddta)
    } else if (!is.null(outlierds)) {
            print(gtxt("Warning: An outlier dataset was requested, but outliers are only available for classification trees:"))
    }
    spssdictionary.EndDataStep()

    if (!retainforest) {
        rm(list=ls())
    } else {
        # preserve objects necessary for scoring and clear the rest of the workspace
        assign("res", res, envir=.GlobalEnv)
        assign("tbl1lbls", tbl1lbls, envir=.GlobalEnv)
        assign("tbl1values", tbl1values, envir=.GlobalEnv)
        assign("spssdict", spssdict, envir=.GlobalEnv)
        rm(list = setdiff(ls(), list("res", "tbl1lbls", "tbl1values", "spssdict")))
    }
}

saveimputeddataset = function(imputeddataset, spssdict, dta, caseid, caseiddta) {
    tryCatch(
        {
        spssdictionary.SetDictionaryToSPSS(imputeddataset, spssdict)
        if (is.null(caseid)) {
          spssdata.SetDataToSPSS(imputeddataset, dta)
        } else {
          spssdata.SetDataToSPSS(imputeddataset, cbind(dta, caseiddta))
        }
        },
        error=function(e) {
            stop(gtxtf("Error creating dataset: %s", imputeddataset), call.=FALSE)
            spssdictionary.EndDataStep()
        })
}

savepreddataset = function(predvalues, spssdict, res, dep, caseid, caseiddta) {
    if (is.null(res$predicted)) {
        print(gtxt("Predicted values are not available in unsupervised mode"))
    } else {
        depdict = spssdict[match(dep, spssdict["varName",])]
        depvarlevel = depdict["varMeasurementLevel",]
        depvarfmt = depdict["varFormat",]
        depvartype = depdict["varType",]
        if (!is.null(caseid)) {
          iddict = spssdict[match(caseid, spssdict["varName",])]
          idvarlevel = depdict["varMeasurementLevel",]
          idvarfmt = depdict["varFormat",]
          idvartype = depdict["varType",]
          caseidlist = c(caseid, gtxt("ID"), idvartype, idvarfmt, idvarlevel)
        }
        
        dlist = list(c("caseNumber_", gtxt("Case Number"), 0, "F8.0", "nominal"),
            c("predictedValues_", gtxtf("predicted Values for %s", dep), depvartype, depvarfmt, depvarlevel))
        if (!is.null(caseid)) {
          dlist[[3]] = caseidlist
        }
        dict = spssdictionary.CreateSPSSDictionary(dlist)
        
        #dict <- spssdictionary.CreateSPSSDictionary(c("caseNumber_", gtxt("Case Number"), 0, "F8.0", "nominal"),
        #c("predictedValues_", gtxtf("predicted Values for %s", dep), depvartype, depvarfmt, depvarlevel))
        #} else {
        #  dict <- spssdictionary.CreateSPSSDictionary(c("caseNumber_", gtxt("Case Number"), 0, "F8.0", "nominal"),
        #    c("predictedValues_", gtxtf("predicted Values for %s", dep), depvartype, depvarfmt, depvarlevel),
        #    c(caseid, "", idvartype, idvarfmt, idvarlevel))
        #}
        tryCatch({
            spssdictionary.SetDictionaryToSPSS(predvalues, dict)
            df = data.frame(res$predicted)
            if (!is.null(caseid)) {
              df = cbind(df, caseiddta)
            }
            spssdata.SetDataToSPSS(predvalues, data.frame(row.names(df), df))},
            error=function(e) {print(e)
            print(gtxtf("Failed to create predicted values dataset: %s.", predvalues))
            spssdictionary.EndDataStep()})
    }
}

saveoutliersdataset = function(outliercases, spssdict, res, dep, dta, caseid, caseiddta) {
    # if a dependent variable was given, whether supervised or not
    # it is used to define classes for the outlier calculation
    # Otherwise everything is considered one class.
    # A dependent variable is optional as is a caseid
    # Only type=classification is allowed

    if (is.null(dep)) {
        label = gtxt("Outlier measure (single class)")
    } else {
        label = gtxtf("Outlier measure (class defined by %s)", dep)
    }
    casenum = c("caseNumber_", gtxt("Case Number"), 0, "F8.0", "nominal")
    outl = c("outlier_", label , 0, "F8.2", "scale")
    dict = list(casenum, outl)
    if (!is.null(dep)) {
      depdict = spssdict[match(dep, spssdict["varName",])]
      depvarlevel = depdict["varMeasurementLevel",]
      depvarfmt = depdict["varFormat",]
      depvartype = depdict["varType",]
      depspec = c(dep, gtxt("Dep Variable"), depvartype, depvarfmt, depvarlevel)
      dict[[3]] = depspec
    } ##else {
      #depspec = NULL
    #}
      
    if (!is.null(caseid)) {
      iddict = spssdict[match(caseid, spssdict["varName",])]
      idvarlevel = depdict["varMeasurementLevel",]
      idvarfmt = depdict["varFormat",]
      idvartype = depdict["varType",]
      caseidspec = c(caseid, gtxt("ID"), idvartype, idvarfmt, idvarlevel)
      dict[[length(dict)+1]] = caseidspec
    } #else {
      #  caseidspec = NULL
    # }
    dict = spssdictionary.CreateSPSSDictionary(dict)
    
    #if (is.null(depspec) && is.null(caseidspec)) {
    #  dict<- spssdictionary.CreateSPSSDictionary(casenum, outl)
    #} else if (!is.null(depspec) && !is.null(caseidspec)) {
    #  dict<- spssdictionary.CreateSPSSDictionary(casenum, outl, depspec, caseidspec)
    #} else if (is.null(depspec)) {
    #  dict<- spssdictionary.CreateSPSSDictionary(casenum, outl, caseidspec)
    #} else {
    #  dict<- spssdictionary.CreateSPSSDictionary(casenum, outl, depspec)
    # }
      
    tryCatch({
      ###save(res, dta, dep, caseidspec, file="c:/temp/ranforws.rdata")   #debug
      spssdictionary.SetDictionaryToSPSS(outliercases, dict)
        if (is.null(dep)) {
            df = data.frame(outlier(res$proximity))
        } else {
            df = data.frame(outlier(res$proximity, cls=res$classes))
        }
        if (!is.null(dep)) {
          if (!is.null(caseid)) {
            spssdata.SetDataToSPSS(outliercases, data.frame(row.names(df), df, dta[dep], caseiddta))
          } else {
              spssdata.SetDataToSPSS(outliercases, data.frame(row.names(df), df, dta[dep]))
          }
        } else {
            if (!is.null(caseid)) {
                spssdata.SetDataToSPSS(outliercases, data.frame(row.names(df), df, caseiddta))
            } else {
                spssdata.SetDataToSPSS(outliercases, data.frame(row.names(df), df, caseiddta))
            }
        }
    }, error=function(e) {print(e)
      print(gtxtf("Failed to create outliers dataset: %s", outliercases))
      spssdictionary.EndDataStep()
        }
      )
    spssdictionary.EndDataStep()
}

# localization initialization
setuplocalization = function() {
    # enable localization		
    domain <- "SPSSINC_RANFOR"
    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
}

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}

gtxt <- function(...) {
    return(gettext(...,domain="SPSSINC_RANFOR"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="SPSSINC_RANFOR"))
}

Run <- function(args) {
    #Execute the SPSSINC RANFOR command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("DEPENDENT", subc="",  ktype="existingvarlist", var="dep", islist=FALSE),
        spsspkg.Template("ENTER", subc="",  ktype="existingvarlist", var="indep", islist=TRUE),
        spsspkg.Template("UNSUPERVISED", subc="", ktype="bool", var="unsupervised"),
        spsspkg.Template("ID", subc="", ktype="existingvarlist", var="caseid", islist=FALSE),
        
        spsspkg.Template("MISSING", subc="OPTIONS",  ktype="str", var="missing", islist=FALSE),
        spsspkg.Template("NUMTREES", subc="OPTIONS",  ktype="int", var="numtrees", islist=FALSE),
        spsspkg.Template("VARSSAMPLED", subc="OPTIONS",  ktype="int", var="varssampled", islist=FALSE),
        spsspkg.Template("MINNODESIZE", subc="OPTIONS",ktype="int", var="minnodesize"),
        spsspkg.Template("CLASSPRIORS", subc="OPTIONS", ktype="float", var="classpriors", islist=TRUE),
        spsspkg.Template("RANDOMNUMBERSEED", subc="OPTIONS", ktype="float", var="seed"),
        
        spsspkg.Template("RETAINFOREST", subc="SAVE", ktype="bool", var="retainforest"),
        spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),
        spsspkg.Template("PREDVALUES", subc="SAVE", ktype="literal", var="predvalues"),
        spsspkg.Template("IMPUTEDDATASET", subc="SAVE", ktype="literal", var="imputeddataset"),
        spsspkg.Template("FOREST", subc="SAVE", ktype="literal", var="forest"),
        spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),
        
        spsspkg.Template("IMPORTANCE",  subc="PRINT", ktype="bool", var="importance"),
        spsspkg.Template("VARUSAGE", subc="PRINT", ktype="bool", var="varusage"),
        
        spsspkg.Template("PARTIALPLOTS", subc="PLOT", ktype="varname", var="partialplots", islist=TRUE),
        spsspkg.Template("MDSPLOT", subc="PLOT", ktype="bool", var="mdsplot"),
        spsspkg.Template("MDSPLOTDIM", subc="PLOT", ktype="int", var="mdsplotdim"),
        spsspkg.Template("VARIABLEIMPORTANCE", subc="PLOT", ktype="bool", var="variableimportance"),
        spsspkg.Template("OUTLIERS", subc="SAVE", ktype="literal", var="outlierds"),
        
        spsspkg.Template("HELP", subc="", ktype="bool")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "ranfor")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}