import QtQuick
import JASP.Module
import "qml/qml_components/SurvivalUpgrade.js" as SurvivalUpgrade
import "qml/qml_components" as UpgradeTools

Upgrades
{
	Upgrade
	{
		functionName: "ParametricSurvivalAnalysis"
		fromVersion: "0.96.7"
		toVersion: "0.97.0"
		msg: qsTr("Parametric survival models now always include an intercept. Results may change if this saved analysis previously excluded the intercept.")

		ChangeRemove
		{
			name: "includeIntercept"
			condition: function(options) { return options["includeIntercept"] !== undefined; }
		}

		ChangeJS
		{
			name: "subgroup"
			condition: function(options) { return options["subgroup"] !== undefined; }
			jsFunction: function(options)
			{
				let subgroup = options["subgroup"];
				if (typeof subgroup === "string")
					return subgroup.length > 0 ? [subgroup] : [];
				if (subgroup && typeof subgroup.value === "string")
					subgroup.value = subgroup.value.length > 0 ? [subgroup.value] : [];
				return subgroup;
			}
		}
	}

	Upgrade
	{
		functionName: "ParametricSurvivalAnalysis"
		fromVersion: "0.97.1"
		toVersion: "0.97.2"
		msg: qsTr("Prediction and diagnostic settings have changed. Please review the updated options and results.")

		// Rename before initializing defaults; saved values take precedence.

		UpgradeTools.UpgradeRenameOption { from: "densityPlotHistogramBinWidthType"; to: "densityPlotObservedDataBinWidthType" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotHistogramManualNumberOfBins"; to: "densityPlotObservedDataNumberOfBins" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotConfidenceInterval"; to: "densityPlotConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotConfidenceIntervalLevel"; to: "densityPlotConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotTransformXAxis"; to: "densityPlotXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotPointCoordinates"; to: "probabilityPlotEmpiricalPointsCoordinates" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotConfidenceInterval"; to: "probabilityPlotConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotConfidenceIntervalLevel"; to: "probabilityPlotConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotTiesHandler"; to: "probabilityPlotTies" }

		UpgradeTools.UpgradeRenameOption { from: "coefficientsConfidenceInterval"; to: "coefficientsConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "coefficientsConfidenceIntervalLevel"; to: "coefficientsConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "modelSummaryAicWeighs"; to: "modelSummaryAicWeights" }

		UpgradeTools.UpgradeRenameOption { from: "modelSummaryBicWeighs"; to: "modelSummaryBicWeights" }

		UpgradeTools.UpgradeRenameOption { from: "advancedSpecification"; to: "modelAdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "advancedSpecificationTerms"; to: "modelAdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameterRegression"; to: "modelAncillaryParameterRegression" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettings"; to: "predictionsCovariateXAxisSettings" }

		UpgradeTools.UpgradeRenameOption { from: "includeFullDatasetInSubgroupAnalysis"; to: "outputFormattingIncludeFullDatasetInSubgroupAnalysis" }

		UpgradeTools.UpgradeRenameOption { from: "compareModelsAcrossDistributions"; to: "outputFormattingCompareModelsAcrossDistributions" }

		UpgradeTools.UpgradeRenameOption { from: "alwaysDisplayModelInformation"; to: "outputFormattingAlwaysDisplayModelInformation" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedMean"; to: "exportFittedValuesMeanSurvivalTime" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedMedian"; to: "exportFittedValuesMedianSurvivalTime" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedRisk"; to: "exportFittedValuesRelativeRisk" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedLinearPredictor"; to: "exportFittedValuesLinearPredictor" }

		UpgradeTools.UpgradeRenameOption { from: "exportMixtureProbabilities"; to: "exportMixtureClassificationComponentProbabilities" }

		UpgradeTools.UpgradeRenameOption { from: "exportMixtureClassification"; to: "exportMixtureClassificationComponentIndex" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotStratify"; to: "densityPlotStratifyBy" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotStratification"; to: "densityPlotStratifyByVariables" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotQuantileGroups"; to: "densityPlotStratifyByQuantiles" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotGroupUniqueLevels"; to: "densityPlotStratifyByGroupUniqueLevels" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotGroupUniqueLevelsLimit"; to: "densityPlotStratifyByGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotStratify"; to: "probabilityPlotStratifyBy" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotStratification"; to: "probabilityPlotStratifyByVariables" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotQuantileGroups"; to: "probabilityPlotStratifyByQuantiles" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotGroupUniqueLevels"; to: "probabilityPlotStratifyByGroupUniqueLevels" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotGroupUniqueLevelsLimit"; to: "probabilityPlotStratifyByGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1ModelTerms"; to: "modelAncillaryParameter1ModelTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1AdvancedSpecification"; to: "modelAncillaryParameter1AdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1AdvancedSpecificationTerms"; to: "modelAncillaryParameter1AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1AvailableTerms"; to: "modelAncillaryParameter1AvailableTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2ModelTerms"; to: "modelAncillaryParameter2ModelTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2AdvancedSpecification"; to: "modelAncillaryParameter2AdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2AdvancedSpecificationTerms"; to: "modelAncillaryParameter2AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2AvailableTerms"; to: "modelAncillaryParameter2AvailableTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3ModelTerms"; to: "modelAncillaryParameter3ModelTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3AdvancedSpecification"; to: "modelAncillaryParameter3AdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3AdvancedSpecificationTerms"; to: "modelAncillaryParameter3AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3AvailableTerms"; to: "modelAncillaryParameter3AvailableTerms" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsXAxisTransformation"; to: "predictionsCovariateXAxisSettingsXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsXAxisFrom"; to: "predictionsCovariateXAxisSettingsXAxisFrom" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsXAxisTo"; to: "predictionsCovariateXAxisSettingsXAxisTo" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsYAxisTransformation"; to: "predictionsCovariateXAxisSettingsYAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsYAxisFrom"; to: "predictionsCovariateXAxisSettingsYAxisFrom" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsYAxisTo"; to: "predictionsCovariateXAxisSettingsYAxisTo" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionExponential"; to: "selectedParametricDistributionsExponential" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGamma"; to: "selectedParametricDistributionsGamma" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedF"; to: "selectedParametricDistributionsGeneralizedF" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedGamma"; to: "selectedParametricDistributionsGeneralizedGamma" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGompertz"; to: "selectedParametricDistributionsGompertz" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionLogLogistic"; to: "selectedParametricDistributionsLogLogistic" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionLogNormal"; to: "selectedParametricDistributionsLogNormal" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionWeibull"; to: "selectedParametricDistributionsWeibull" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedGammaOriginal"; to: "selectedParametricDistributionsGeneralizedGammaOriginal" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedFOriginal"; to: "selectedParametricDistributionsGeneralizedFOriginal" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsConfidenceInterval"; to: "predictionsDisplayConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsConfidenceIntervalLevel"; to: "predictionsDisplayConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotKaplanMeier"; to: "predictionsDisplayKaplanMeier" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotCensoringEvents"; to: "predictionsDisplayCensoringEvents" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotTransformXAxis"; to: "predictionsXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotTransformYAxis"; to: "predictionsProbabilityYAxisScale" }

		UpgradeTools.UpgradeRenameOption { from: "lifeTimeMergeTablesAcrossMeasures"; to: "predictionsMergeTablesAcrossMeasures" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsLifeTimeRoundSteps"; to: "predictionsRoundTimeTableValues" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotMergePlotsAcrossFactors"; to: "probabilityPlotMergePlotsAcrossStrata" }

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeSurvivalTime"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "survivalTime"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeSurvivalProbability"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "survivalProbability"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeFailureProbability"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "failureProbability"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeHazard"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "hazard"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeCumulativeHazard"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "cumulativeHazard"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeRestrictedMeanSurvivalTime"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "restrictedMeanSurvivalTime"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutputPlot"
			jsFunction: function(options) { return SurvivalUpgrade.output(options, "Plot"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutputTable"
			jsFunction: function(options) { return SurvivalUpgrade.output(options, "Table"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsPredictFor"
			jsFunction: function(options) { return "variables"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutputPlotType"
			jsFunction: function(options) { return "standard"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentiles"
			jsFunction: function(options) { return options["predictionsOutcomeSurvivalTime"] === true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsMergeSeriesAcrossDistributions"
			jsFunction: function(options) { return SurvivalUpgrade.merge(options, "Distributions"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsMergeSeriesAcrossSubgroups"
			jsFunction: function(options) { return SurvivalUpgrade.merge(options, "Subgroups"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimeTimeId"
			jsFunction: function(options) { return "\uE000"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsXAxisVariables"
			jsFunction: function(options) { return Array.isArray(options["modelTerms"]) ? {value: options["predictionsLifetimeTimeId"], types: "unknown"} : {shouldEncode: true}; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsSeparateLines"
			jsFunction: function(options) { return SurvivalUpgrade.factorAssignments(options, false); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsSeparatePlots"
			jsFunction: function(options) { return SurvivalUpgrade.factorAssignments(options, true); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsAssignedCovariateValuesQuantiles"
			jsFunction: function(options) { return 3; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsRolesAutomatic"
			jsFunction: function(options) { return !SurvivalUpgrade.separateFactors(options); }
		}

		// Probability fractions become percentiles; table time counts become intervals.

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesGrid"
			condition: function(options) { return options["predictionsLifetimePercentilesGrid"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileGrid(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesIntervals"
			condition: function(options) { return options["predictionsLifetimePercentilesIntervals"] === undefined && options["predictionsSurvivalTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsSurvivalTimeStepsNumber"]; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesFrom"
			condition: function(options) { return options["predictionsLifetimePercentilesFrom"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileSequence(options, "From"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesStep"
			condition: function(options) { return options["predictionsLifetimePercentilesStep"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileSequence(options, "Step"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesTo"
			condition: function(options) { return options["predictionsLifetimePercentilesTo"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileSequence(options, "To"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesValues"
			condition: function(options) { return options["predictionsLifetimePercentilesValues"] === undefined && options["predictionsSurvivalTimeCustom"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileValues(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeGrid"
			condition: function(options) { return options["predictionsTimeGrid"] === undefined && options["predictionsLifeTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.timeGrid(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeGrid"
			condition: function(options) { return options["predictionsTimeGrid"] === "custom"; }
			jsFunction: function(options) { return "customAbsolute"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeIntervals"
			condition: function(options) { return options["predictionsTimeIntervals"] === undefined && options["predictionsLifeTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsLifeTimeStepsNumber"] - 1; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeFrom"
			condition: function(options) { return options["predictionsTimeFrom"] === undefined && options["predictionsLifeTimeStepsFrom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsFrom"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeStep"
			condition: function(options) { return options["predictionsTimeStep"] === undefined && options["predictionsLifeTimeStepsSize"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsSize"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeTo"
			condition: function(options) { return options["predictionsTimeTo"] === undefined && options["predictionsLifeTimeStepsTo"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsTo"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeValues"
			condition: function(options) { return options["predictionsTimeValues"] === undefined && options["predictionsLifeTimeCustom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeCustom"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "probabilityPlotStratifyBy"
			jsFunction: function(options) { return true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "probabilityPlotStratifyByVariables"
			jsFunction: function(options) { return SurvivalUpgrade.factorStrata(options); }
		}

		ChangeSetValue
		{
			name: "confidenceIntervalBounds"
			condition: function(options) { return options["confidenceIntervalBounds"] === undefined; }
			jsonValue: "twoSided"
		}

		ChangeSetValue
		{
			name: "fittedModelEquation"
			condition: function(options) { return options["fittedModelEquation"] === undefined; }
			jsonValue: false
		}
		ChangeSetValue
		{
			name: "fittedModelEquationDecimals"
			condition: function(options) { return options["fittedModelEquationDecimals"] === undefined; }
			jsonValue: 3
		}

		ChangeSetValue
		{
			name: "modelAncillaryParameterRegression"
			condition: function(options) { return options["modelAncillaryParameterRegression"] === undefined; }
			jsonValue: false
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter1ModelTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return options["modelTerms"].map(function(model) {
					return {name: "ancillary1" + model.name, title: model.title, components: {value: [], types: []}};
				});
			}
		}
		ChangeSetValue
		{
			name: "modelAncillaryParameter1AdvancedSpecification"
			condition: function(options) { return options["modelAncillaryParameter1AdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary1" + model.name, predictors: []}; });
			}
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter2ModelTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return options["modelTerms"].map(function(model) {
					return {name: "ancillary2" + model.name, title: model.title, components: {value: [], types: []}};
				});
			}
		}
		ChangeSetValue
		{
			name: "modelAncillaryParameter2AdvancedSpecification"
			condition: function(options) { return options["modelAncillaryParameter2AdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary2" + model.name, predictors: []}; });
			}
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter3ModelTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return options["modelTerms"].map(function(model) {
					return {name: "ancillary3" + model.name, title: model.title, components: {value: [], types: []}};
				});
			}
		}
		ChangeSetValue
		{
			name: "modelAncillaryParameter3AdvancedSpecification"
			condition: function(options) { return options["modelAncillaryParameter3AdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary3" + model.name, predictors: []}; });
			}
		}

		ChangeSetValue
		{
			name: "modelAdvancedSpecification"
			condition: function(options) { return options["modelAdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		ChangeSetValue
		{
			name: "restrictParameters"
			condition: function(options) { return options["restrictParameters"] === undefined; }
			jsonValue: false
		}
		ChangeSetValue
		{
			name: "restrictParametersValues"
			condition: function(options) { return options["restrictParametersValues"] === undefined; }
			jsonValue: []
		}
		// Consume old options only after every dependent setting has been converted.

		UpgradeTools.UpgradeRemoveOption { name: "advancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "advancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "alwaysDisplayModelInformation" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1AdvancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1AvailableTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1ModelTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2AdvancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2AvailableTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2ModelTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3AdvancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3AvailableTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3ModelTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameterRegression" }

		UpgradeTools.UpgradeRemoveOption { name: "coefficientsConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "coefficientsConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "compareModelsAcrossDistributions" }

		UpgradeTools.UpgradeRemoveOption { name: "cumulativeHazardPlot" }

		UpgradeTools.UpgradeRemoveOption { name: "cumulativeHazardTable" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotGroupUniqueLevels" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotHistogramBinWidthType" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotHistogramManualNumberOfBins" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotQuantileGroups" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotStratification" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotStratify" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotTransformXAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedLinearPredictor" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedMean" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedMedian" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedRisk" }

		UpgradeTools.UpgradeRemoveOption { name: "exportMixtureClassification" }

		UpgradeTools.UpgradeRemoveOption { name: "exportMixtureProbabilities" }

		UpgradeTools.UpgradeRemoveOption { name: "hazardPlot" }

		UpgradeTools.UpgradeRemoveOption { name: "hazardTable" }

		UpgradeTools.UpgradeRemoveOption { name: "includeFullDatasetInSubgroupAnalysis" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossDistributions" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossSubgroups" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergeTablesAcrossMeasures" }

		UpgradeTools.UpgradeRemoveOption { name: "modelSummaryAicWeighs" }

		UpgradeTools.UpgradeRemoveOption { name: "modelSummaryBicWeighs" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettings" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsXAxisFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsXAxisTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsXAxisTransformation" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsYAxisFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsYAxisTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsYAxisTransformation" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeCustom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeRoundSteps" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsNumber" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsSize" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsType" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeCustom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsNumber" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsSize" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsType" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotGroupUniqueLevels" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotPointCoordinates" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotQuantileGroups" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotStratification" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotStratify" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotTiesHandler" }

		UpgradeTools.UpgradeRemoveOption { name: "restrictedMeanSurvivalTimePlot" }

		UpgradeTools.UpgradeRemoveOption { name: "restrictedMeanSurvivalTimeTable" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionExponential" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGamma" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedF" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedFOriginal" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedGamma" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedGammaOriginal" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGompertz" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionLogLogistic" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionLogNormal" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionWeibull" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityAsFailureProbability" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlot" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotCensoringEvents" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotKaplanMeier" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotTransformXAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotTransformYAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityTable" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossDistributions" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossSubgroups" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimePlot" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeTable" }

	}

	Upgrade
	{
		functionName: "ParametricMixtureSurvivalAnalysis"
		fromVersion: "0.97.1"
		toVersion: "0.97.2"
		msg: qsTr("Prediction, diagnostic and starting-value settings have changed. Please review the updated options and results.")

		// Older Quantiles combined equal-sized and tail partitions. Preserve
		// that grouping only when the saved file has no separate tail setting.
		UpgradeTools.UpgradeNewOption
		{
			name: "mixtureStartTails"
			jsFunction: function(options)
			{
				if (options["mixtureStartQuantiles"] !== undefined)
					return options["mixtureStartQuantiles"] === true;
				if (options["mixtureStartingValuesQuantiles"] !== undefined)
					return options["mixtureStartingValuesQuantiles"] === true;
				return true;
			}
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "mixtureStartMembershipProbabilities"
			jsFunction: function(options) { return "0.95"; }
		}

		// Rename before initializing defaults; saved values take precedence.

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentsTable"; to: "mixtureMeanAndMedian" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureClassificationTable"; to: "mixtureClassification" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureDiagnosticsTable"; to: "mixtureEstimationDiagnostics" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotTransformXAxis"; to: "mixtureComponentPlotXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotTransformYAxis"; to: "mixtureComponentPlotYAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureStartKmeans"; to: "mixtureStartingValuesKMeans" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureStartQuantiles"; to: "mixtureStartingValuesQuantiles" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureStartSplit"; to: "mixtureStartingValuesSplitOfThePreviousSolution" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureStartRandom"; to: "mixtureStartingValuesRandom" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureStartRandomCount"; to: "mixtureStartingValuesRandomCount" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotHistogramBinWidthType"; to: "densityPlotObservedDataBinWidthType" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotHistogramManualNumberOfBins"; to: "densityPlotObservedDataNumberOfBins" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotConfidenceInterval"; to: "densityPlotConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotConfidenceIntervalLevel"; to: "densityPlotConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotTransformXAxis"; to: "densityPlotXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotPointCoordinates"; to: "probabilityPlotEmpiricalPointsCoordinates" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotConfidenceInterval"; to: "probabilityPlotConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotConfidenceIntervalLevel"; to: "probabilityPlotConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotTiesHandler"; to: "probabilityPlotTies" }

		UpgradeTools.UpgradeRenameOption { from: "coefficientsConfidenceInterval"; to: "coefficientsConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "coefficientsConfidenceIntervalLevel"; to: "coefficientsConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "modelSummaryAicWeighs"; to: "modelSummaryAicWeights" }

		UpgradeTools.UpgradeRenameOption { from: "modelSummaryBicWeighs"; to: "modelSummaryBicWeights" }

		UpgradeTools.UpgradeRenameOption { from: "advancedSpecification"; to: "modelAdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "advancedSpecificationTerms"; to: "modelAdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameterRegression"; to: "modelAncillaryParameterRegression" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettings"; to: "predictionsCovariateXAxisSettings" }

		UpgradeTools.UpgradeRenameOption { from: "includeFullDatasetInSubgroupAnalysis"; to: "outputFormattingIncludeFullDatasetInSubgroupAnalysis" }

		UpgradeTools.UpgradeRenameOption { from: "compareModelsAcrossDistributions"; to: "outputFormattingCompareModelsAcrossDistributions" }

		UpgradeTools.UpgradeRenameOption { from: "compareModelsAcrossComponents"; to: "outputFormattingCompareModelsAcrossComponents" }

		UpgradeTools.UpgradeRenameOption { from: "alwaysDisplayModelInformation"; to: "outputFormattingAlwaysDisplayModelInformation" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedMean"; to: "exportFittedValuesMeanSurvivalTime" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedMedian"; to: "exportFittedValuesMedianSurvivalTime" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedRisk"; to: "exportFittedValuesRelativeRisk" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedLinearPredictor"; to: "exportFittedValuesLinearPredictor" }

		UpgradeTools.UpgradeRenameOption { from: "exportMixtureProbabilities"; to: "exportMixtureClassificationComponentProbabilities" }

		UpgradeTools.UpgradeRenameOption { from: "exportMixtureClassification"; to: "exportMixtureClassificationComponentIndex" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotStratify"; to: "densityPlotStratifyBy" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotStratification"; to: "densityPlotStratifyByVariables" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotQuantileGroups"; to: "densityPlotStratifyByQuantiles" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotGroupUniqueLevels"; to: "densityPlotStratifyByGroupUniqueLevels" }

		UpgradeTools.UpgradeRenameOption { from: "densityPlotGroupUniqueLevelsLimit"; to: "densityPlotStratifyByGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotStratify"; to: "probabilityPlotStratifyBy" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotStratification"; to: "probabilityPlotStratifyByVariables" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotQuantileGroups"; to: "probabilityPlotStratifyByQuantiles" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotGroupUniqueLevels"; to: "probabilityPlotStratifyByGroupUniqueLevels" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotGroupUniqueLevelsLimit"; to: "probabilityPlotStratifyByGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotStratify"; to: "mixtureComponentPlotStratifyBy" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotStratification"; to: "mixtureComponentPlotStratifyByVariables" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotQuantileGroups"; to: "mixtureComponentPlotStratifyByQuantiles" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotGroupUniqueLevels"; to: "mixtureComponentPlotStratifyByGroupUniqueLevels" }

		UpgradeTools.UpgradeRenameOption { from: "mixtureComponentPlotGroupUniqueLevelsLimit"; to: "mixtureComponentPlotStratifyByGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1ModelTerms"; to: "modelAncillaryParameter1ModelTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1AdvancedSpecification"; to: "modelAncillaryParameter1AdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1AdvancedSpecificationTerms"; to: "modelAncillaryParameter1AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter1AvailableTerms"; to: "modelAncillaryParameter1AvailableTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2ModelTerms"; to: "modelAncillaryParameter2ModelTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2AdvancedSpecification"; to: "modelAncillaryParameter2AdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2AdvancedSpecificationTerms"; to: "modelAncillaryParameter2AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter2AvailableTerms"; to: "modelAncillaryParameter2AvailableTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3ModelTerms"; to: "modelAncillaryParameter3ModelTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3AdvancedSpecification"; to: "modelAncillaryParameter3AdvancedSpecification" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3AdvancedSpecificationTerms"; to: "modelAncillaryParameter3AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRenameOption { from: "ancillaryParameter3AvailableTerms"; to: "modelAncillaryParameter3AvailableTerms" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsXAxisTransformation"; to: "predictionsCovariateXAxisSettingsXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsXAxisFrom"; to: "predictionsCovariateXAxisSettingsXAxisFrom" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsXAxisTo"; to: "predictionsCovariateXAxisSettingsXAxisTo" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsYAxisTransformation"; to: "predictionsCovariateXAxisSettingsYAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsYAxisFrom"; to: "predictionsCovariateXAxisSettingsYAxisFrom" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsCovariateAxisSettingsYAxisTo"; to: "predictionsCovariateXAxisSettingsYAxisTo" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionExponential"; to: "selectedParametricDistributionsExponential" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGamma"; to: "selectedParametricDistributionsGamma" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedF"; to: "selectedParametricDistributionsGeneralizedF" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedGamma"; to: "selectedParametricDistributionsGeneralizedGamma" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGompertz"; to: "selectedParametricDistributionsGompertz" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionLogLogistic"; to: "selectedParametricDistributionsLogLogistic" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionLogNormal"; to: "selectedParametricDistributionsLogNormal" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionWeibull"; to: "selectedParametricDistributionsWeibull" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedGammaOriginal"; to: "selectedParametricDistributionsGeneralizedGammaOriginal" }

		UpgradeTools.UpgradeRenameOption { from: "selectedParametricDistributionGeneralizedFOriginal"; to: "selectedParametricDistributionsGeneralizedFOriginal" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsConfidenceInterval"; to: "predictionsDisplayConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsConfidenceIntervalLevel"; to: "predictionsDisplayConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotKaplanMeier"; to: "predictionsDisplayKaplanMeier" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotCensoringEvents"; to: "predictionsDisplayCensoringEvents" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotTransformXAxis"; to: "predictionsXAxisTransformation" }

		UpgradeTools.UpgradeRenameOption { from: "survivalProbabilityPlotTransformYAxis"; to: "predictionsProbabilityYAxisScale" }

		UpgradeTools.UpgradeRenameOption { from: "lifeTimeMergeTablesAcrossMeasures"; to: "predictionsMergeTablesAcrossMeasures" }

		UpgradeTools.UpgradeRenameOption { from: "predictionsLifeTimeRoundSteps"; to: "predictionsRoundTimeTableValues" }

		UpgradeTools.UpgradeRenameOption { from: "probabilityPlotMergePlotsAcrossFactors"; to: "probabilityPlotMergePlotsAcrossStrata" }

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			condition: function(options) { return SurvivalUpgrade.hasOldModifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return SurvivalUpgrade.modifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeSurvivalTime"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "survivalTime"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeSurvivalProbability"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "survivalProbability"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeFailureProbability"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "failureProbability"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeHazard"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "hazard"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeCumulativeHazard"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "cumulativeHazard"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutcomeRestrictedMeanSurvivalTime"
			jsFunction: function(options) { return SurvivalUpgrade.outcome(options, "restrictedMeanSurvivalTime"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutputPlot"
			jsFunction: function(options) { return SurvivalUpgrade.output(options, "Plot"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutputTable"
			jsFunction: function(options) { return SurvivalUpgrade.output(options, "Table"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsPredictFor"
			jsFunction: function(options) { return "variables"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsOutputPlotType"
			jsFunction: function(options) { return "standard"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentiles"
			jsFunction: function(options) { return options["predictionsOutcomeSurvivalTime"] === true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsMergeSeriesAcrossDistributions"
			jsFunction: function(options) { return SurvivalUpgrade.merge(options, "Distributions"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsMergeSeriesAcrossSubgroups"
			jsFunction: function(options) { return SurvivalUpgrade.merge(options, "Subgroups"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsMergeSeriesAcrossComponents"
			jsFunction: function(options) { return SurvivalUpgrade.merge(options, "Components"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimeTimeId"
			jsFunction: function(options) { return "\uE000"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsXAxisVariables"
			jsFunction: function(options) { return Array.isArray(options["modelTerms"]) ? {value: options["predictionsLifetimeTimeId"], types: "unknown"} : {shouldEncode: true}; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsSeparateLines"
			jsFunction: function(options) { return SurvivalUpgrade.factorAssignments(options, false); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsSeparatePlots"
			jsFunction: function(options) { return SurvivalUpgrade.factorAssignments(options, true); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsAssignedCovariateValuesQuantiles"
			jsFunction: function(options) { return 3; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsRolesAutomatic"
			jsFunction: function(options) { return !SurvivalUpgrade.separateFactors(options); }
		}

		// Probability fractions become percentiles; table time counts become intervals.

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesGrid"
			condition: function(options) { return options["predictionsLifetimePercentilesGrid"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileGrid(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesIntervals"
			condition: function(options) { return options["predictionsLifetimePercentilesIntervals"] === undefined && options["predictionsSurvivalTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsSurvivalTimeStepsNumber"]; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesFrom"
			condition: function(options) { return options["predictionsLifetimePercentilesFrom"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileSequence(options, "From"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesStep"
			condition: function(options) { return options["predictionsLifetimePercentilesStep"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileSequence(options, "Step"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesTo"
			condition: function(options) { return options["predictionsLifetimePercentilesTo"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileSequence(options, "To"); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsLifetimePercentilesValues"
			condition: function(options) { return options["predictionsLifetimePercentilesValues"] === undefined && options["predictionsSurvivalTimeCustom"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.percentileValues(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeGrid"
			condition: function(options) { return options["predictionsTimeGrid"] === undefined && options["predictionsLifeTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.timeGrid(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeGrid"
			condition: function(options) { return options["predictionsTimeGrid"] === "custom"; }
			jsFunction: function(options) { return "customAbsolute"; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeIntervals"
			condition: function(options) { return options["predictionsTimeIntervals"] === undefined && options["predictionsLifeTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsLifeTimeStepsNumber"] - 1; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeFrom"
			condition: function(options) { return options["predictionsTimeFrom"] === undefined && options["predictionsLifeTimeStepsFrom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsFrom"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeStep"
			condition: function(options) { return options["predictionsTimeStep"] === undefined && options["predictionsLifeTimeStepsSize"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsSize"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeTo"
			condition: function(options) { return options["predictionsTimeTo"] === undefined && options["predictionsLifeTimeStepsTo"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsTo"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "predictionsTimeValues"
			condition: function(options) { return options["predictionsTimeValues"] === undefined && options["predictionsLifeTimeCustom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeCustom"]); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "probabilityPlotStratifyBy"
			jsFunction: function(options) { return true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "probabilityPlotStratifyByVariables"
			jsFunction: function(options) { return SurvivalUpgrade.factorStrata(options); }
		}

		// Observed histograms now belong to the separate density diagnostic.

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlot"
			jsFunction: function(options) { return SurvivalUpgrade.observedDensity(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotObservedData"
			condition: function(options) { return options["densityPlotObservedData"] === undefined && options["mixtureComponentPlotObservedData"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotObservedData"] === true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotXAxisTransformation"
			condition: function(options) { return options["densityPlotXAxisTransformation"] === undefined && options["mixtureComponentPlotXAxisTransformation"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotXAxisTransformation"]; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotMergePlotsAcrossDistributions"
			condition: function(options) { return options["densityPlotMergePlotsAcrossDistributions"] === undefined && options["mixtureComponentPlotMergePlotsAcrossDistributions"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotMergePlotsAcrossDistributions"] === true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotConfidenceIntervals"
			condition: function(options) { return options["densityPlotConfidenceIntervals"] === undefined && options["predictionsDisplayConfidenceIntervals"] !== undefined; }
			jsFunction: function(options) { return options["predictionsDisplayConfidenceIntervals"] === true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotConfidenceIntervalsLevel"
			condition: function(options) { return options["densityPlotConfidenceIntervalsLevel"] === undefined && options["predictionsDisplayConfidenceIntervalsLevel"] !== undefined; }
			jsFunction: function(options) { return options["predictionsDisplayConfidenceIntervalsLevel"]; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotObservedDataBinWidthType"
			condition: function(options) { return options["densityPlotObservedDataBinWidthType"] === undefined && options["mixtureComponentPlotHistogramBinWidthType"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotHistogramBinWidthType"]; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotObservedDataNumberOfBins"
			condition: function(options) { return options["densityPlotObservedDataNumberOfBins"] === undefined && options["mixtureComponentPlotHistogramManualNumberOfBins"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotHistogramManualNumberOfBins"]; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotStratifyBy"
			condition: function(options) { return options["densityPlotStratifyBy"] === undefined && options["mixtureComponentPlotObservedData"] !== undefined; }
			jsFunction: function(options) { return true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "densityPlotStratifyByVariables"
			condition: function(options) { return options["densityPlotStratifyByVariables"] === undefined && options["mixtureComponentPlotObservedData"] !== undefined; }
			jsFunction: function(options) { return SurvivalUpgrade.factorStrata(options); }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "mixtureComponentPlotStratifyBy"
			jsFunction: function(options) { return true; }
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "mixtureComponentPlotStratifyByVariables"
			jsFunction: function(options) { return SurvivalUpgrade.factorStrata(options); }
		}

		ChangeSetValue
		{
			name: "confidenceIntervalBounds"
			condition: function(options) { return options["confidenceIntervalBounds"] === undefined; }
			jsonValue: "twoSided"
		}

		ChangeSetValue
		{
			name: "fittedModelEquation"
			condition: function(options) { return options["fittedModelEquation"] === undefined; }
			jsonValue: false
		}
		ChangeSetValue
		{
			name: "fittedModelEquationDecimals"
			condition: function(options) { return options["fittedModelEquationDecimals"] === undefined; }
			jsonValue: 3
		}

		ChangeSetValue
		{
			name: "modelAncillaryParameterRegression"
			condition: function(options) { return options["modelAncillaryParameterRegression"] === undefined; }
			jsonValue: false
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter1ModelTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return options["modelTerms"].map(function(model) {
					return {name: "ancillary1" + model.name, title: model.title, components: {value: [], types: []}};
				});
			}
		}
		ChangeSetValue
		{
			name: "modelAncillaryParameter1AdvancedSpecification"
			condition: function(options) { return options["modelAncillaryParameter1AdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary1" + model.name, predictors: []}; });
			}
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter2ModelTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return options["modelTerms"].map(function(model) {
					return {name: "ancillary2" + model.name, title: model.title, components: {value: [], types: []}};
				});
			}
		}
		ChangeSetValue
		{
			name: "modelAncillaryParameter2AdvancedSpecification"
			condition: function(options) { return options["modelAncillaryParameter2AdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary2" + model.name, predictors: []}; });
			}
		}

		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter3ModelTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return options["modelTerms"].map(function(model) {
					return {name: "ancillary3" + model.name, title: model.title, components: {value: [], types: []}};
				});
			}
		}
		ChangeSetValue
		{
			name: "modelAncillaryParameter3AdvancedSpecification"
			condition: function(options) { return options["modelAncillaryParameter3AdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		UpgradeTools.UpgradeNewOption
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary3" + model.name, predictors: []}; });
			}
		}

		ChangeSetValue
		{
			name: "modelAdvancedSpecification"
			condition: function(options) { return options["modelAdvancedSpecification"] === undefined; }
			jsonValue: false
		}
		ChangeSetValue
		{
			name: "restrictParameters"
			condition: function(options) { return options["restrictParameters"] === undefined; }
			jsonValue: false
		}
		ChangeSetValue
		{
			name: "restrictParametersValues"
			condition: function(options) { return options["restrictParametersValues"] === undefined; }
			jsonValue: []
		}
		// Consume old options only after every dependent setting has been converted.

		UpgradeTools.UpgradeRemoveOption { name: "advancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "advancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "alwaysDisplayModelInformation" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1AdvancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1AvailableTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter1ModelTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2AdvancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2AvailableTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter2ModelTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3AdvancedSpecification" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3AdvancedSpecificationTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3AvailableTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameter3ModelTerms" }

		UpgradeTools.UpgradeRemoveOption { name: "ancillaryParameterRegression" }

		UpgradeTools.UpgradeRemoveOption { name: "coefficientsConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "coefficientsConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "compareModelsAcrossComponents" }

		UpgradeTools.UpgradeRemoveOption { name: "compareModelsAcrossDistributions" }

		UpgradeTools.UpgradeRemoveOption { name: "cumulativeHazardPlot" }

		UpgradeTools.UpgradeRemoveOption { name: "cumulativeHazardTable" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotGroupUniqueLevels" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotHistogramBinWidthType" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotHistogramManualNumberOfBins" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotQuantileGroups" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotStratification" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotStratify" }

		UpgradeTools.UpgradeRemoveOption { name: "densityPlotTransformXAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedLinearPredictor" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedMean" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedMedian" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedRisk" }

		UpgradeTools.UpgradeRemoveOption { name: "exportMixtureClassification" }

		UpgradeTools.UpgradeRemoveOption { name: "exportMixtureProbabilities" }

		UpgradeTools.UpgradeRemoveOption { name: "hazardPlot" }

		UpgradeTools.UpgradeRemoveOption { name: "hazardTable" }

		UpgradeTools.UpgradeRemoveOption { name: "includeFullDatasetInSubgroupAnalysis" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossComponents" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossDistributions" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergePlotsAcrossSubgroups" }

		UpgradeTools.UpgradeRemoveOption { name: "lifeTimeMergeTablesAcrossMeasures" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureClassificationTable" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotGroupUniqueLevels" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotHistogramBinWidthType" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotHistogramManualNumberOfBins" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotObservedData" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotQuantileGroups" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotStratification" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotStratify" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotTransformXAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentPlotTransformYAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureComponentsTable" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureDiagnosticsTable" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureStartKmeans" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureStartQuantiles" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureStartRandom" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureStartRandomCount" }

		UpgradeTools.UpgradeRemoveOption { name: "mixtureStartSplit" }

		UpgradeTools.UpgradeRemoveOption { name: "modelSummaryAicWeighs" }

		UpgradeTools.UpgradeRemoveOption { name: "modelSummaryBicWeighs" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettings" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsXAxisFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsXAxisTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsXAxisTransformation" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsYAxisFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsYAxisTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsCovariateAxisSettingsYAxisTransformation" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeCustom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeRoundSteps" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsNumber" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsSize" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsLifeTimeStepsType" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeCustom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsFrom" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsNumber" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsSize" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsTo" }

		UpgradeTools.UpgradeRemoveOption { name: "predictionsSurvivalTimeStepsType" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotGroupUniqueLevels" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotGroupUniqueLevelsLimit" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotPointCoordinates" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotQuantileGroups" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotStratification" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotStratify" }

		UpgradeTools.UpgradeRemoveOption { name: "probabilityPlotTiesHandler" }

		UpgradeTools.UpgradeRemoveOption { name: "restrictedMeanSurvivalTimePlot" }

		UpgradeTools.UpgradeRemoveOption { name: "restrictedMeanSurvivalTimeTable" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionExponential" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGamma" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedF" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedFOriginal" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedGamma" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGeneralizedGammaOriginal" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionGompertz" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionLogLogistic" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionLogNormal" }

		UpgradeTools.UpgradeRemoveOption { name: "selectedParametricDistributionWeibull" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityAsFailureProbability" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlot" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotCensoringEvents" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotKaplanMeier" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotTransformXAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityPlotTransformYAxis" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalProbabilityTable" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossComponents" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossDistributions" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossFactors" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeMergePlotsAcrossSubgroups" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimePlot" }

		UpgradeTools.UpgradeRemoveOption { name: "survivalTimeTable" }

	}

	Upgrade
	{
		functionName: "SemiParametricSurvivalAnalysis"
		fromVersion: "0.97.1"
		toVersion: "0.97.2"

		// Rename before initializing defaults; saved values take precedence.

		UpgradeTools.UpgradeRenameOption { from: "coefficientsConfidenceInterval"; to: "coefficientsConfidenceIntervals" }

		UpgradeTools.UpgradeRenameOption { from: "coefficientsConfidenceIntervalLevel"; to: "coefficientsConfidenceIntervalsLevel" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedMean"; to: "exportFittedValuesMeanSurvivalTime" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedMedian"; to: "exportFittedValuesMedianSurvivalTime" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedRisk"; to: "exportFittedValuesRelativeRisk" }

		UpgradeTools.UpgradeRenameOption { from: "exportFittedLinearPredictor"; to: "exportFittedValuesLinearPredictor" }

		UpgradeTools.UpgradeRenameOption { from: "exportMixtureProbabilities"; to: "exportMixtureClassificationComponentProbabilities" }

		UpgradeTools.UpgradeRenameOption { from: "exportMixtureClassification"; to: "exportMixtureClassificationComponentIndex" }

		// Consume old options only after every dependent setting has been converted.

		UpgradeTools.UpgradeRemoveOption { name: "coefficientsConfidenceInterval" }

		UpgradeTools.UpgradeRemoveOption { name: "coefficientsConfidenceIntervalLevel" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedLinearPredictor" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedMean" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedMedian" }

		UpgradeTools.UpgradeRemoveOption { name: "exportFittedRisk" }

		UpgradeTools.UpgradeRemoveOption { name: "exportMixtureClassification" }

		UpgradeTools.UpgradeRemoveOption { name: "exportMixtureProbabilities" }

	}
}
