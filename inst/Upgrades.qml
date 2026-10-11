import QtQuick
import JASP.Module

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
		fromVersion: "0.97.0"
		toVersion: "0.97.1"

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalTime"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeSurvivalTime"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeFailureProbability"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeFailureProbability"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalProbabilityAsFailureProbability"] === true &&
					(options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["predictionsOutcomeSurvivalProbability"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalProbability"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeSurvivalProbability"] === undefined || options["survivalProbabilityAsFailureProbability"] !== undefined; }
			jsFunction: function(options)
			{
				let selected = options["predictionsOutcomeSurvivalProbability"] === true || options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true;
				return selected && options["survivalProbabilityAsFailureProbability"] !== true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeHazard"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeHazard"] === undefined; }
			jsFunction: function(options)
			{
				return options["hazardPlot"] === true || options["hazardTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeCumulativeHazard"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeCumulativeHazard"] === undefined; }
			jsFunction: function(options)
			{
				return options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeRestrictedMeanSurvivalTime"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeRestrictedMeanSurvivalTime"] === undefined; }
			jsFunction: function(options)
			{
				return options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutputPlot"
			isNewOption: true
			condition: function(options) { return options["predictionsOutputPlot"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalTimePlot"] === true || options["survivalProbabilityPlot"] === true || options["hazardPlot"] === true || options["cumulativeHazardPlot"] === true || options["restrictedMeanSurvivalTimePlot"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutputTable"
			isNewOption: true
			condition: function(options) { return options["predictionsOutputTable"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalTimeTable"] === true || options["survivalProbabilityTable"] === true || options["hazardTable"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossDistributions"
			isNewOption: true
			condition: function(options) { return options["predictionsMergeSeriesAcrossDistributions"] === undefined; }
			jsFunction: function(options)
			{
				let time = options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
				let life = options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["hazardPlot"] === true || options["hazardTable"] === true || options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
				return (time && options["survivalTimeMergePlotsAcrossDistributions"] === true) || (life && options["lifeTimeMergePlotsAcrossDistributions"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossSubgroups"
			isNewOption: true
			condition: function(options) { return options["predictionsMergeSeriesAcrossSubgroups"] === undefined; }
			jsFunction: function(options)
			{
				let time = options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
				let life = options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["hazardPlot"] === true || options["hazardTable"] === true || options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
				return (time && options["survivalTimeMergePlotsAcrossSubgroups"] === true) || (life && options["lifeTimeMergePlotsAcrossSubgroups"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossComponents"
			isNewOption: true
			condition: function(options) { return options["predictionsMergeSeriesAcrossComponents"] === undefined; }
			jsFunction: function(options)
			{
				let time = options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
				let life = options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["hazardPlot"] === true || options["hazardTable"] === true || options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
				return (time && options["survivalTimeMergePlotsAcrossComponents"] === true) || (life && options["lifeTimeMergePlotsAcrossComponents"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsLifetimeTimeId"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimeTimeId"] === undefined; }
			jsFunction: function(options)
			{
				return "\uE000";
			}
		}

		ChangeJS
		{
			name: "predictionsXAxisVariables"
			isNewOption: true
			condition: function(options) { return options["predictionsXAxisVariables"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return {value: options["predictionsLifetimeTimeId"], types: "unknown"};
			}
		}

		ChangeJS
		{
			name: "predictionsSeparateLines"
			isNewOption: true
			condition: function(options) { return options["predictionsSeparateLines"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				let factors = options["factors"];
				let values = factors && factors.value !== undefined ? factors.value : factors;
				values = Array.isArray(values) ? values : (values ? [values] : []);
				let predictors = [];
				for (let model of options["modelTerms"]) {
					let terms = model.components && model.components.value;
					if (!Array.isArray(terms)) continue;
					for (let term of terms) for (let v of (Array.isArray(term) ? term : [term])) if (predictors.indexOf(v) < 0) predictors.push(v);
				}
				values = values.filter(function(v) { return predictors.indexOf(v) >= 0; });
				return {value: values, types: values.map(function() { return "nominal"; })};
			}
		}

		ChangeJS
		{
			name: "predictionsSeparatePlots"
			isNewOption: true
			condition: function(options) { return options["predictionsSeparatePlots"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return {value: [], types: []};
			}
		}

		ChangeJS
		{
			name: "predictionsAssignedCovariateValuesQuantiles"
			isNewOption: true
			condition: function(options) { return options["predictionsAssignedCovariateValuesQuantiles"] === undefined; }
			jsFunction: function(options)
			{
				return 3;
			}
		}

		ChangeJS
		{
			name: "predictionsRolesAutomatic"
			isNewOption: true
			condition: function(options) { return options["predictionsRolesAutomatic"] === undefined; }
			jsFunction: function(options)
			{
				return true;
			}
		}

		ChangeRemove { name: "survivalTimePlot"; condition: function(options) { return options["survivalTimePlot"] !== undefined; } }

		ChangeRemove { name: "survivalTimeTable"; condition: function(options) { return options["survivalTimeTable"] !== undefined; } }

		ChangeRemove { name: "survivalProbabilityPlot"; condition: function(options) { return options["survivalProbabilityPlot"] !== undefined; } }

		ChangeRemove { name: "survivalProbabilityTable"; condition: function(options) { return options["survivalProbabilityTable"] !== undefined; } }

		ChangeRemove { name: "survivalProbabilityAsFailureProbability"; condition: function(options) { return options["survivalProbabilityAsFailureProbability"] !== undefined; } }

		ChangeRemove { name: "hazardPlot"; condition: function(options) { return options["hazardPlot"] !== undefined; } }

		ChangeRemove { name: "hazardTable"; condition: function(options) { return options["hazardTable"] !== undefined; } }

		ChangeRemove { name: "cumulativeHazardPlot"; condition: function(options) { return options["cumulativeHazardPlot"] !== undefined; } }

		ChangeRemove { name: "cumulativeHazardTable"; condition: function(options) { return options["cumulativeHazardTable"] !== undefined; } }

		ChangeRemove { name: "restrictedMeanSurvivalTimePlot"; condition: function(options) { return options["restrictedMeanSurvivalTimePlot"] !== undefined; } }

		ChangeRemove { name: "restrictedMeanSurvivalTimeTable"; condition: function(options) { return options["restrictedMeanSurvivalTimeTable"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossDistributions"; condition: function(options) { return options["survivalTimeMergePlotsAcrossDistributions"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossSubgroups"; condition: function(options) { return options["survivalTimeMergePlotsAcrossSubgroups"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossComponents"; condition: function(options) { return options["survivalTimeMergePlotsAcrossComponents"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossFactors"; condition: function(options) { return options["survivalTimeMergePlotsAcrossFactors"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossDistributions"; condition: function(options) { return options["lifeTimeMergePlotsAcrossDistributions"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossSubgroups"; condition: function(options) { return options["lifeTimeMergePlotsAcrossSubgroups"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossComponents"; condition: function(options) { return options["lifeTimeMergePlotsAcrossComponents"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossFactors"; condition: function(options) { return options["lifeTimeMergePlotsAcrossFactors"] !== undefined; } }

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

		ChangeJS
		{
			name: "modelAncillaryParameter1ModelTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter1ModelTerms"] === undefined; }
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
		ChangeJS
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter1AdvancedSpecificationTerms"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary1" + model.name, predictors: []}; });
			}
		}

		ChangeJS
		{
			name: "modelAncillaryParameter2ModelTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter2ModelTerms"] === undefined; }
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
		ChangeJS
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter2AdvancedSpecificationTerms"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary2" + model.name, predictors: []}; });
			}
		}

		ChangeJS
		{
			name: "modelAncillaryParameter3ModelTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter3ModelTerms"] === undefined; }
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
		ChangeJS
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter3AdvancedSpecificationTerms"] === undefined; }
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
		// Preserve released evaluation/display settings under their current names.
		ChangeRename
		{
			from: "predictionsConfidenceInterval"
			to: "predictionsDisplayConfidenceIntervals"
		}
		ChangeRename
		{
			from: "predictionsConfidenceIntervalLevel"
			to: "predictionsDisplayConfidenceIntervalsLevel"
		}
		ChangeRename
		{
			from: "survivalProbabilityPlotKaplanMeier"
			to: "predictionsDisplayKaplanMeier"
		}
		ChangeRename
		{
			from: "survivalProbabilityPlotCensoringEvents"
			to: "predictionsDisplayCensoringEvents"
		}
		ChangeRename
		{
			from: "lifeTimeMergeTablesAcrossMeasures"
			to: "predictionsMergeTablesAcrossMeasures"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeRoundSteps"
			to: "predictionsRoundTimeTableValues"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsType"
			to: "predictionsLifetimePercentilesGrid"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsNumber"
			to: "predictionsLifetimePercentilesIntervals"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsFrom"
			to: "predictionsLifetimePercentilesFrom"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsSize"
			to: "predictionsLifetimePercentilesStep"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsTo"
			to: "predictionsLifetimePercentilesTo"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeCustom"
			to: "predictionsLifetimePercentilesValues"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsType"
			to: "predictionsTimeGrid"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsNumber"
			to: "predictionsTimeIntervals"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsFrom"
			to: "predictionsTimeFrom"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsSize"
			to: "predictionsTimeStep"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsTo"
			to: "predictionsTimeTo"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeCustom"
			to: "predictionsTimeValues"
		}

		// Preserve released settings after aligning option names with labels.
		ChangeRename
		{
			from: "probabilityPlotPointCoordinates"
			to: "probabilityPlotEmpiricalPointsCoordinates"
		}
		ChangeRename
		{
			from: "probabilityPlotConfidenceInterval"
			to: "probabilityPlotConfidenceIntervals"
		}
		ChangeRename
		{
			from: "probabilityPlotConfidenceIntervalLevel"
			to: "probabilityPlotConfidenceIntervalsLevel"
		}
		ChangeRename
		{
			from: "probabilityPlotTiesHandler"
			to: "probabilityPlotTies"
		}
		ChangeRename
		{
			from: "coefficientsConfidenceInterval"
			to: "coefficientsConfidenceIntervals"
		}
		ChangeRename
		{
			from: "coefficientsConfidenceIntervalLevel"
			to: "coefficientsConfidenceIntervalsLevel"
		}
		ChangeRename
		{
			from: "modelSummaryAicWeighs"
			to: "modelSummaryAicWeights"
		}
		ChangeRename
		{
			from: "modelSummaryBicWeighs"
			to: "modelSummaryBicWeights"
		}
		ChangeRename
		{
			from: "includeFullDatasetInSubgroupAnalysis"
			to: "outputFormattingIncludeFullDatasetInSubgroupAnalysis"
		}
		ChangeRename
		{
			from: "compareModelsAcrossDistributions"
			to: "outputFormattingCompareModelsAcrossDistributions"
		}
		ChangeRename
		{
			from: "alwaysDisplayModelInformation"
			to: "outputFormattingAlwaysDisplayModelInformation"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionExponential"
			to: "selectedParametricDistributionsExponential"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGamma"
			to: "selectedParametricDistributionsGamma"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedF"
			to: "selectedParametricDistributionsGeneralizedF"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGamma"
			to: "selectedParametricDistributionsGeneralizedGamma"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGompertz"
			to: "selectedParametricDistributionsGompertz"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionLogLogistic"
			to: "selectedParametricDistributionsLogLogistic"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionLogNormal"
			to: "selectedParametricDistributionsLogNormal"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionWeibull"
			to: "selectedParametricDistributionsWeibull"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGammaOriginal"
			to: "selectedParametricDistributionsGeneralizedGammaOriginal"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedFOriginal"
			to: "selectedParametricDistributionsGeneralizedFOriginal"
		}

	}

	Upgrade
	{
		functionName: "ParametricMixtureSurvivalAnalysis"
		fromVersion: "0.97.0"
		toVersion: "0.97.1"

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalTime"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeSurvivalTime"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeFailureProbability"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeFailureProbability"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalProbabilityAsFailureProbability"] === true &&
					(options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["predictionsOutcomeSurvivalProbability"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalProbability"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeSurvivalProbability"] === undefined || options["survivalProbabilityAsFailureProbability"] !== undefined; }
			jsFunction: function(options)
			{
				let selected = options["predictionsOutcomeSurvivalProbability"] === true || options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true;
				return selected && options["survivalProbabilityAsFailureProbability"] !== true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeHazard"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeHazard"] === undefined; }
			jsFunction: function(options)
			{
				return options["hazardPlot"] === true || options["hazardTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeCumulativeHazard"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeCumulativeHazard"] === undefined; }
			jsFunction: function(options)
			{
				return options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutcomeRestrictedMeanSurvivalTime"
			isNewOption: true
			condition: function(options) { return options["predictionsOutcomeRestrictedMeanSurvivalTime"] === undefined; }
			jsFunction: function(options)
			{
				return options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutputPlot"
			isNewOption: true
			condition: function(options) { return options["predictionsOutputPlot"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalTimePlot"] === true || options["survivalProbabilityPlot"] === true || options["hazardPlot"] === true || options["cumulativeHazardPlot"] === true || options["restrictedMeanSurvivalTimePlot"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsOutputTable"
			isNewOption: true
			condition: function(options) { return options["predictionsOutputTable"] === undefined; }
			jsFunction: function(options)
			{
				return options["survivalTimeTable"] === true || options["survivalProbabilityTable"] === true || options["hazardTable"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
			}
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossDistributions"
			isNewOption: true
			condition: function(options) { return options["predictionsMergeSeriesAcrossDistributions"] === undefined; }
			jsFunction: function(options)
			{
				let time = options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
				let life = options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["hazardPlot"] === true || options["hazardTable"] === true || options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
				return (time && options["survivalTimeMergePlotsAcrossDistributions"] === true) || (life && options["lifeTimeMergePlotsAcrossDistributions"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossSubgroups"
			isNewOption: true
			condition: function(options) { return options["predictionsMergeSeriesAcrossSubgroups"] === undefined; }
			jsFunction: function(options)
			{
				let time = options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
				let life = options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["hazardPlot"] === true || options["hazardTable"] === true || options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
				return (time && options["survivalTimeMergePlotsAcrossSubgroups"] === true) || (life && options["lifeTimeMergePlotsAcrossSubgroups"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossComponents"
			isNewOption: true
			condition: function(options) { return options["predictionsMergeSeriesAcrossComponents"] === undefined; }
			jsFunction: function(options)
			{
				let time = options["survivalTimePlot"] === true || options["survivalTimeTable"] === true;
				let life = options["survivalProbabilityPlot"] === true || options["survivalProbabilityTable"] === true || options["hazardPlot"] === true || options["hazardTable"] === true || options["cumulativeHazardPlot"] === true || options["cumulativeHazardTable"] === true || options["restrictedMeanSurvivalTimePlot"] === true || options["restrictedMeanSurvivalTimeTable"] === true;
				return (time && options["survivalTimeMergePlotsAcrossComponents"] === true) || (life && options["lifeTimeMergePlotsAcrossComponents"] === true);
			}
		}

		ChangeJS
		{
			name: "predictionsLifetimeTimeId"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimeTimeId"] === undefined; }
			jsFunction: function(options)
			{
				return "\uE000";
			}
		}

		ChangeJS
		{
			name: "predictionsXAxisVariables"
			isNewOption: true
			condition: function(options) { return options["predictionsXAxisVariables"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return {value: options["predictionsLifetimeTimeId"], types: "unknown"};
			}
		}

		ChangeJS
		{
			name: "predictionsSeparateLines"
			isNewOption: true
			condition: function(options) { return options["predictionsSeparateLines"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				let factors = options["factors"];
				let values = factors && factors.value !== undefined ? factors.value : factors;
				values = Array.isArray(values) ? values : (values ? [values] : []);
				let predictors = [];
				for (let model of options["modelTerms"]) {
					let terms = model.components && model.components.value;
					if (!Array.isArray(terms)) continue;
					for (let term of terms) for (let v of (Array.isArray(term) ? term : [term])) if (predictors.indexOf(v) < 0) predictors.push(v);
				}
				values = values.filter(function(v) { return predictors.indexOf(v) >= 0; });
				return {value: values, types: values.map(function() { return "nominal"; })};
			}
		}

		ChangeJS
		{
			name: "predictionsSeparatePlots"
			isNewOption: true
			condition: function(options) { return options["predictionsSeparatePlots"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
				return {value: [], types: []};
			}
		}

		ChangeJS
		{
			name: "predictionsAssignedCovariateValuesQuantiles"
			isNewOption: true
			condition: function(options) { return options["predictionsAssignedCovariateValuesQuantiles"] === undefined; }
			jsFunction: function(options)
			{
				return 3;
			}
		}

		ChangeJS
		{
			name: "predictionsRolesAutomatic"
			isNewOption: true
			condition: function(options) { return options["predictionsRolesAutomatic"] === undefined; }
			jsFunction: function(options)
			{
				return true;
			}
		}

		ChangeRemove { name: "survivalTimePlot"; condition: function(options) { return options["survivalTimePlot"] !== undefined; } }

		ChangeRemove { name: "survivalTimeTable"; condition: function(options) { return options["survivalTimeTable"] !== undefined; } }

		ChangeRemove { name: "survivalProbabilityPlot"; condition: function(options) { return options["survivalProbabilityPlot"] !== undefined; } }

		ChangeRemove { name: "survivalProbabilityTable"; condition: function(options) { return options["survivalProbabilityTable"] !== undefined; } }

		ChangeRemove { name: "survivalProbabilityAsFailureProbability"; condition: function(options) { return options["survivalProbabilityAsFailureProbability"] !== undefined; } }

		ChangeRemove { name: "hazardPlot"; condition: function(options) { return options["hazardPlot"] !== undefined; } }

		ChangeRemove { name: "hazardTable"; condition: function(options) { return options["hazardTable"] !== undefined; } }

		ChangeRemove { name: "cumulativeHazardPlot"; condition: function(options) { return options["cumulativeHazardPlot"] !== undefined; } }

		ChangeRemove { name: "cumulativeHazardTable"; condition: function(options) { return options["cumulativeHazardTable"] !== undefined; } }

		ChangeRemove { name: "restrictedMeanSurvivalTimePlot"; condition: function(options) { return options["restrictedMeanSurvivalTimePlot"] !== undefined; } }

		ChangeRemove { name: "restrictedMeanSurvivalTimeTable"; condition: function(options) { return options["restrictedMeanSurvivalTimeTable"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossDistributions"; condition: function(options) { return options["survivalTimeMergePlotsAcrossDistributions"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossSubgroups"; condition: function(options) { return options["survivalTimeMergePlotsAcrossSubgroups"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossComponents"; condition: function(options) { return options["survivalTimeMergePlotsAcrossComponents"] !== undefined; } }

		ChangeRemove { name: "survivalTimeMergePlotsAcrossFactors"; condition: function(options) { return options["survivalTimeMergePlotsAcrossFactors"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossDistributions"; condition: function(options) { return options["lifeTimeMergePlotsAcrossDistributions"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossSubgroups"; condition: function(options) { return options["lifeTimeMergePlotsAcrossSubgroups"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossComponents"; condition: function(options) { return options["lifeTimeMergePlotsAcrossComponents"] !== undefined; } }

		ChangeRemove { name: "lifeTimeMergePlotsAcrossFactors"; condition: function(options) { return options["lifeTimeMergePlotsAcrossFactors"] !== undefined; } }

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

		ChangeJS
		{
			name: "modelAncillaryParameter1ModelTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter1ModelTerms"] === undefined; }
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
		ChangeJS
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter1AdvancedSpecificationTerms"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary1" + model.name, predictors: []}; });
			}
		}

		ChangeJS
		{
			name: "modelAncillaryParameter2ModelTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter2ModelTerms"] === undefined; }
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
		ChangeJS
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter2AdvancedSpecificationTerms"] === undefined; }
			jsFunction: function(options)
			{
				if (!Array.isArray(options["modelTerms"])) return {predictors: {shouldEncode: true}};
				return options["modelTerms"].map(function(model) { return {model: "ancillary2" + model.name, predictors: []}; });
			}
		}

		ChangeJS
		{
			name: "modelAncillaryParameter3ModelTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter3ModelTerms"] === undefined; }
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
		ChangeJS
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return options["modelAncillaryParameter3AdvancedSpecificationTerms"] === undefined; }
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
		// Preserve released evaluation/display settings under their current names.
		ChangeRename
		{
			from: "predictionsConfidenceInterval"
			to: "predictionsDisplayConfidenceIntervals"
		}
		ChangeRename
		{
			from: "predictionsConfidenceIntervalLevel"
			to: "predictionsDisplayConfidenceIntervalsLevel"
		}
		ChangeRename
		{
			from: "survivalProbabilityPlotKaplanMeier"
			to: "predictionsDisplayKaplanMeier"
		}
		ChangeRename
		{
			from: "survivalProbabilityPlotCensoringEvents"
			to: "predictionsDisplayCensoringEvents"
		}
		ChangeRename
		{
			from: "lifeTimeMergeTablesAcrossMeasures"
			to: "predictionsMergeTablesAcrossMeasures"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeRoundSteps"
			to: "predictionsRoundTimeTableValues"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsType"
			to: "predictionsLifetimePercentilesGrid"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsNumber"
			to: "predictionsLifetimePercentilesIntervals"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsFrom"
			to: "predictionsLifetimePercentilesFrom"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsSize"
			to: "predictionsLifetimePercentilesStep"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeStepsTo"
			to: "predictionsLifetimePercentilesTo"
		}
		ChangeRename
		{
			from: "predictionsSurvivalTimeCustom"
			to: "predictionsLifetimePercentilesValues"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsType"
			to: "predictionsTimeGrid"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsNumber"
			to: "predictionsTimeIntervals"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsFrom"
			to: "predictionsTimeFrom"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsSize"
			to: "predictionsTimeStep"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeStepsTo"
			to: "predictionsTimeTo"
		}
		ChangeRename
		{
			from: "predictionsLifeTimeCustom"
			to: "predictionsTimeValues"
		}

		// Preserve released settings after aligning option names with labels.
		ChangeRename
		{
			from: "probabilityPlotPointCoordinates"
			to: "probabilityPlotEmpiricalPointsCoordinates"
		}
		ChangeRename
		{
			from: "probabilityPlotConfidenceInterval"
			to: "probabilityPlotConfidenceIntervals"
		}
		ChangeRename
		{
			from: "probabilityPlotConfidenceIntervalLevel"
			to: "probabilityPlotConfidenceIntervalsLevel"
		}
		ChangeRename
		{
			from: "probabilityPlotTiesHandler"
			to: "probabilityPlotTies"
		}
		ChangeRename
		{
			from: "coefficientsConfidenceInterval"
			to: "coefficientsConfidenceIntervals"
		}
		ChangeRename
		{
			from: "coefficientsConfidenceIntervalLevel"
			to: "coefficientsConfidenceIntervalsLevel"
		}
		ChangeRename
		{
			from: "modelSummaryAicWeighs"
			to: "modelSummaryAicWeights"
		}
		ChangeRename
		{
			from: "modelSummaryBicWeighs"
			to: "modelSummaryBicWeights"
		}
		ChangeRename
		{
			from: "includeFullDatasetInSubgroupAnalysis"
			to: "outputFormattingIncludeFullDatasetInSubgroupAnalysis"
		}
		ChangeRename
		{
			from: "compareModelsAcrossDistributions"
			to: "outputFormattingCompareModelsAcrossDistributions"
		}
		ChangeRename
		{
			from: "alwaysDisplayModelInformation"
			to: "outputFormattingAlwaysDisplayModelInformation"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionExponential"
			to: "selectedParametricDistributionsExponential"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGamma"
			to: "selectedParametricDistributionsGamma"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedF"
			to: "selectedParametricDistributionsGeneralizedF"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGamma"
			to: "selectedParametricDistributionsGeneralizedGamma"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGompertz"
			to: "selectedParametricDistributionsGompertz"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionLogLogistic"
			to: "selectedParametricDistributionsLogLogistic"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionLogNormal"
			to: "selectedParametricDistributionsLogNormal"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionWeibull"
			to: "selectedParametricDistributionsWeibull"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGammaOriginal"
			to: "selectedParametricDistributionsGeneralizedGammaOriginal"
		}
		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedFOriginal"
			to: "selectedParametricDistributionsGeneralizedFOriginal"
		}

	}
}

