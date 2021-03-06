###### The number of models to be generated (0 for all models)
numModels = 10

###### The minimal number of iterations for generalization
minIterationsGeneralize = 1

###### The maximal number of iterations for generalization
maxIterationsGeneralize = 20

###### The minimal number of iterations for blending
# minIterationsBlend = 1

###### The maximal number of iterations for blending
# maxIterationsBlend = 20

###### Percentage of blend value below the currently highest value found so far that we want to keep in the results. 0 means that we only keep the blends with the highest value found so far, 100 means to consider blends with half the value, etc.. Setting this to -sys.maxint-1 means to consider all blends.
blendValuePercentageBelowHighestValueToKeep = 25
# blendCostPercentageAboveMinToKeep = sys.maxint

###### Time limit for eprover and darwin consistency check in seconds CPU time
eproverTimeLimit = 5
darwinTimeLimit = 2

###### Path to the HETS executable ######
hetsExe = 'hets'
# hetsExe = '/media/psf/Home/svn/coinvent/Amalgamation/hetsPrio' # This is the experimental version with priorities by Mihai.

###### Switch to enable the explicit generation of blend files (see function writeBlends.py) ######
genExplicitBlendFiles = True


############################
###### CASL-specific #######
############################

###### For generalization, determine whether removal of axioms, sorts, predicates and operators are allowed atomic generalization actions. For certain domains it may be useful to use only a subset of generalization operations.
# TODO: THis is currently not working... For now (de)-comment respective LP rules in generalize.lp file to select generalization operations
# rmAxAllowed = True
# rmPredAllowed = True
# rmOpAllowed = True
# rmSortAllowed = True

###################################################################

## Here is space to quickly overwrite the above settings for debugging purposes.
# inputFile = "examples/cadenceByAx.casl"
# inputSpaceNames = ["Perfect7Cadence","PhrygianCadence"]

# inputFile = "examples/houseBoat.casl"
# inputSpaceNames = ["Boat","House"]

# inputFile = "examples/LPNMR/cadenceBlendFusionTest.casl"
# inputSpaceNames = ["PhrygianCadence","PerfectCadence"]
