import QtQuick
import JASP.Module

Upgrades
{
	id: upgrades

	// Shared saved-option conversions; all migration rules stay in this file.
	function selected(options, measure)
	{
		return options[measure + "Plot"] === true || options[measure + "Table"] === true;
	}

	function lifeSelected(options)
	{
		return ["survivalProbability", "hazard", "cumulativeHazard", "restrictedMeanSurvivalTime"].some(function(measure) { return selected(options, measure); });
	}

	function outcome(options, measure)
	{
		if (measure === "failureProbability")
			return selected(options, "survivalProbability") && options["survivalProbabilityAsFailureProbability"] === true;
		if (measure === "survivalProbability")
			return selected(options, measure) && options["survivalProbabilityAsFailureProbability"] !== true;
		return selected(options, measure);
	}

	function output(options, kind)
	{
		return ["survivalTime", "survivalProbability", "hazard", "cumulativeHazard", "restrictedMeanSurvivalTime"].some(function(measure) { return options[measure + kind] === true; });
	}

	function merge(options, suffix)
	{
		return (selected(options, "survivalTime") && options["survivalTimeMergePlotsAcross" + suffix] === true) ||
			(lifeSelected(options) && options["lifeTimeMergePlotsAcross" + suffix] === true);
	}

	function values(option)
	{
		let value = option && option.value !== undefined ? option.value : option;
		return Array.isArray(value) ? value : (typeof value === "string" && value.length ? [value] : []);
	}

	function modelFactors(options)
	{
		let predictors = [];
		for (let model of options["modelTerms"] || [])
		{
			let terms = values(model.components);
			for (let term of terms)
				for (let variable of (Array.isArray(term) ? term : [term]))
					if (predictors.indexOf(variable) < 0) predictors.push(variable);
		}
		return values(options["factors"]).filter(function(variable) { return predictors.indexOf(variable) >= 0; });
	}

	function separateFactors(options)
	{
		// Time-evaluated curves take precedence when both old plot families were selected.
		return lifeSelected(options) ? options["lifeTimeMergePlotsAcrossFactors"] === false :
			selected(options, "survivalTime") && options["survivalTimeMergePlotsAcrossFactors"] === false;
	}

	function factorAssignments(options, panels)
	{
		if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
		let factors = separateFactors(options) === panels ? modelFactors(options) : [];
		return {value: factors, types: factors.map(function() { return "nominal"; })};
	}

	function hasOldModifierNames(value)
	{
		if (!value || typeof value !== "object") return false;
		return Object.keys(value).some(function(key) {
			return key === "factorReferenceLevel" || key === "predictorTransformationTemperatureUnit" || hasOldModifierNames(value[key]);
		});
	}

	function modifierNames(value)
	{
		if (!value || typeof value !== "object") return value;
		if (Array.isArray(value)) return value.map(modifierNames);
		let renamed = {};
		for (let key of Object.keys(value))
		{
			let target = key === "factorReferenceLevel" ? "factorReference" :
				key === "predictorTransformationTemperatureUnit" ? "predictorTransformationFrom" : key;
			if (target !== key && value[target] !== undefined) continue;
			renamed[target] = modifierNames(value[key]);
		}
		return renamed;
	}

	function percentileGrid(options)
	{
		// The old equal grid excluded the 100th percentile; retain its finite endpoint.
		return options["predictionsSurvivalTimeStepsType"] === "quantiles" ? "sequence" : options["predictionsSurvivalTimeStepsType"];
	}

	function percentileSequence(options, field)
	{
		if (options["predictionsSurvivalTimeStepsType"] === "quantiles")
		{
			let number = options["predictionsSurvivalTimeStepsNumber"];
			if (field === "From") return "0";
			return String(field === "Step" ? 100 / number : 100 * (number - 1) / number);
		}
		let oldField = field === "Step" ? "Size" : field;
		let value = options["predictionsSurvivalTimeSteps" + oldField];
		if (value === undefined) return "";
		if (field === "To" && Number(value) === 1)
		{
			let from = Number(options["predictionsSurvivalTimeStepsFrom"]);
			let step = Number(options["predictionsSurvivalTimeStepsSize"]);
			if (step > 0)
				value = from + (Math.ceil((1 - from) / step) - 1) * step;
		}
		return String(100 * Number(value));
	}

	function percentileValues(options)
	{
		let value = options["predictionsSurvivalTimeCustom"];
		if (value === undefined) return "";
		let text = String(value).trim().replace(/^c\s*\(/, "").replace(/\)$/, "");
		let numbers = text.split(",").filter(function(part) { return part.trim().length > 0; }).map(Number);
		if (!numbers.length || numbers.some(function(number) { return !isFinite(number); })) return String(value);
		return numbers.map(function(number) { return String(100 * number); }).join(", ");
	}

	function timeGrid(options)
	{
		return options["predictionsLifeTimeStepsType"] === "custom" ? "customAbsolute" : options["predictionsLifeTimeStepsType"];
	}

	function factorStrata(options)
	{
		if (!Array.isArray(options["modelTerms"])) return {shouldEncode: true};
		return modelFactors(options).map(function(variable) { return {variable: {value: variable, types: "nominal"}, stratify: true}; });
	}

	function observedDensity(options)
	{
		return options["mixtureComponentPlot"] === true && options["mixtureComponentPlotObservedData"] === true && options["mixtureComponentPlotType"] === "density";
	}

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

		ChangeRename
		{
			from: "densityPlotHistogramBinWidthType"
			to: "densityPlotObservedDataBinWidthType"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotHistogramManualNumberOfBins"
			to: "densityPlotObservedDataNumberOfBins"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotConfidenceInterval"
			to: "densityPlotConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotConfidenceIntervalLevel"
			to: "densityPlotConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotTransformXAxis"
			to: "densityPlotXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotPointCoordinates"
			to: "probabilityPlotEmpiricalPointsCoordinates"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotConfidenceInterval"
			to: "probabilityPlotConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotConfidenceIntervalLevel"
			to: "probabilityPlotConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotTiesHandler"
			to: "probabilityPlotTies"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "coefficientsConfidenceInterval"
			to: "coefficientsConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "coefficientsConfidenceIntervalLevel"
			to: "coefficientsConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "modelSummaryAicWeighs"
			to: "modelSummaryAicWeights"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "modelSummaryBicWeighs"
			to: "modelSummaryBicWeights"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "advancedSpecification"
			to: "modelAdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "advancedSpecificationTerms"
			to: "modelAdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameterRegression"
			to: "modelAncillaryParameterRegression"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettings"
			to: "predictionsCovariateXAxisSettings"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "includeFullDatasetInSubgroupAnalysis"
			to: "outputFormattingIncludeFullDatasetInSubgroupAnalysis"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "compareModelsAcrossDistributions"
			to: "outputFormattingCompareModelsAcrossDistributions"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "alwaysDisplayModelInformation"
			to: "outputFormattingAlwaysDisplayModelInformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedMean"
			to: "exportFittedValuesMeanSurvivalTime"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedMedian"
			to: "exportFittedValuesMedianSurvivalTime"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedRisk"
			to: "exportFittedValuesRelativeRisk"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedLinearPredictor"
			to: "exportFittedValuesLinearPredictor"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportMixtureProbabilities"
			to: "exportMixtureClassificationComponentProbabilities"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportMixtureClassification"
			to: "exportMixtureClassificationComponentIndex"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotStratify"
			to: "densityPlotStratifyBy"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotStratification"
			to: "densityPlotStratifyByVariables"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotQuantileGroups"
			to: "densityPlotStratifyByQuantiles"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotGroupUniqueLevels"
			to: "densityPlotStratifyByGroupUniqueLevels"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotGroupUniqueLevelsLimit"
			to: "densityPlotStratifyByGroupUniqueLevelsLimit"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotStratify"
			to: "probabilityPlotStratifyBy"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotStratification"
			to: "probabilityPlotStratifyByVariables"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotQuantileGroups"
			to: "probabilityPlotStratifyByQuantiles"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotGroupUniqueLevels"
			to: "probabilityPlotStratifyByGroupUniqueLevels"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotGroupUniqueLevelsLimit"
			to: "probabilityPlotStratifyByGroupUniqueLevelsLimit"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1ModelTerms"
			to: "modelAncillaryParameter1ModelTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1AdvancedSpecification"
			to: "modelAncillaryParameter1AdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1AdvancedSpecificationTerms"
			to: "modelAncillaryParameter1AdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1AvailableTerms"
			to: "modelAncillaryParameter1AvailableTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2ModelTerms"
			to: "modelAncillaryParameter2ModelTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2AdvancedSpecification"
			to: "modelAncillaryParameter2AdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2AdvancedSpecificationTerms"
			to: "modelAncillaryParameter2AdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2AvailableTerms"
			to: "modelAncillaryParameter2AvailableTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3ModelTerms"
			to: "modelAncillaryParameter3ModelTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3AdvancedSpecification"
			to: "modelAncillaryParameter3AdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3AdvancedSpecificationTerms"
			to: "modelAncillaryParameter3AdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3AvailableTerms"
			to: "modelAncillaryParameter3AvailableTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsXAxisTransformation"
			to: "predictionsCovariateXAxisSettingsXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsXAxisFrom"
			to: "predictionsCovariateXAxisSettingsXAxisFrom"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsXAxisTo"
			to: "predictionsCovariateXAxisSettingsXAxisTo"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsYAxisTransformation"
			to: "predictionsCovariateXAxisSettingsYAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsYAxisFrom"
			to: "predictionsCovariateXAxisSettingsYAxisFrom"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsYAxisTo"
			to: "predictionsCovariateXAxisSettingsYAxisTo"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionExponential"
			to: "selectedParametricDistributionsExponential"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGamma"
			to: "selectedParametricDistributionsGamma"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedF"
			to: "selectedParametricDistributionsGeneralizedF"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGamma"
			to: "selectedParametricDistributionsGeneralizedGamma"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGompertz"
			to: "selectedParametricDistributionsGompertz"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionLogLogistic"
			to: "selectedParametricDistributionsLogLogistic"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionLogNormal"
			to: "selectedParametricDistributionsLogNormal"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionWeibull"
			to: "selectedParametricDistributionsWeibull"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGammaOriginal"
			to: "selectedParametricDistributionsGeneralizedGammaOriginal"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedFOriginal"
			to: "selectedParametricDistributionsGeneralizedFOriginal"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsConfidenceInterval"
			to: "predictionsDisplayConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsConfidenceIntervalLevel"
			to: "predictionsDisplayConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotKaplanMeier"
			to: "predictionsDisplayKaplanMeier"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotCensoringEvents"
			to: "predictionsDisplayCensoringEvents"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotTransformXAxis"
			to: "predictionsXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotTransformYAxis"
			to: "predictionsProbabilityYAxisScale"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "lifeTimeMergeTablesAcrossMeasures"
			to: "predictionsMergeTablesAcrossMeasures"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsLifeTimeRoundSteps"
			to: "predictionsRoundTimeTableValues"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotMergePlotsAcrossFactors"
			to: "probabilityPlotMergePlotsAcrossStrata"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeJS
		{
			name: "modelAdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalTime"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "survivalTime"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalProbability"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "survivalProbability"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeFailureProbability"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "failureProbability"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeHazard"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "hazard"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeCumulativeHazard"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "cumulativeHazard"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeRestrictedMeanSurvivalTime"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "restrictedMeanSurvivalTime"); }
		}

		ChangeJS
		{
			name: "predictionsOutputPlot"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.output(options, "Plot"); }
		}

		ChangeJS
		{
			name: "predictionsOutputTable"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.output(options, "Table"); }
		}

		ChangeJS
		{
			name: "predictionsPredictFor"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "variables"; }
		}

		ChangeJS
		{
			name: "predictionsOutputPlotType"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "standard"; }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentiles"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return options["predictionsOutcomeSurvivalTime"] === true; }
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossDistributions"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.merge(options, "Distributions"); }
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossSubgroups"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.merge(options, "Subgroups"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimeTimeId"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "\uE000"; }
		}

		ChangeJS
		{
			name: "predictionsXAxisVariables"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return Array.isArray(options["modelTerms"]) ? {value: options["predictionsLifetimeTimeId"], types: "unknown"} : {shouldEncode: true}; }
		}

		ChangeJS
		{
			name: "predictionsSeparateLines"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorAssignments(options, false); }
		}

		ChangeJS
		{
			name: "predictionsSeparatePlots"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorAssignments(options, true); }
		}

		ChangeJS
		{
			name: "predictionsAssignedCovariateValuesQuantiles"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return 3; }
		}

		ChangeJS
		{
			name: "predictionsRolesAutomatic"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return !upgrades.separateFactors(options); }
		}

		// Probability fractions become percentiles; table time counts become intervals.

		ChangeJS
		{
			name: "predictionsLifetimePercentilesGrid"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesGrid"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileGrid(options); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesIntervals"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesIntervals"] === undefined && options["predictionsSurvivalTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsSurvivalTimeStepsNumber"]; }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesFrom"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesFrom"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileSequence(options, "From"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesStep"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesStep"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileSequence(options, "Step"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesTo"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesTo"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileSequence(options, "To"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesValues"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesValues"] === undefined && options["predictionsSurvivalTimeCustom"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileValues(options); }
		}

		ChangeJS
		{
			name: "predictionsTimeGrid"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeGrid"] === undefined && options["predictionsLifeTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.timeGrid(options); }
		}

		ChangeJS
		{
			name: "predictionsTimeGrid"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeGrid"] === "custom"; }
			jsFunction: function(options) { return "customAbsolute"; }
		}

		ChangeJS
		{
			name: "predictionsTimeIntervals"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeIntervals"] === undefined && options["predictionsLifeTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsLifeTimeStepsNumber"] - 1; }
		}

		ChangeJS
		{
			name: "predictionsTimeFrom"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeFrom"] === undefined && options["predictionsLifeTimeStepsFrom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsFrom"]); }
		}

		ChangeJS
		{
			name: "predictionsTimeStep"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeStep"] === undefined && options["predictionsLifeTimeStepsSize"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsSize"]); }
		}

		ChangeJS
		{
			name: "predictionsTimeTo"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeTo"] === undefined && options["predictionsLifeTimeStepsTo"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsTo"]); }
		}

		ChangeJS
		{
			name: "predictionsTimeValues"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeValues"] === undefined && options["predictionsLifeTimeCustom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeCustom"]); }
		}

		ChangeJS
		{
			name: "probabilityPlotStratifyBy"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return true; }
		}

		ChangeJS
		{
			name: "probabilityPlotStratifyByVariables"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorStrata(options); }
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

		ChangeJS
		{
			name: "modelAncillaryParameter1ModelTerms"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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

		ChangeRemove
		{
			name: "advancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "advancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "alwaysDisplayModelInformation"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1AdvancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1AdvancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1AvailableTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1ModelTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2AdvancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2AdvancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2AvailableTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2ModelTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3AdvancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3AdvancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3AvailableTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3ModelTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameterRegression"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "coefficientsConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "coefficientsConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "compareModelsAcrossDistributions"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "cumulativeHazardPlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "cumulativeHazardTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotGroupUniqueLevels"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotGroupUniqueLevelsLimit"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotHistogramBinWidthType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotHistogramManualNumberOfBins"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotQuantileGroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotStratification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotStratify"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotTransformXAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedLinearPredictor"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedMean"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedMedian"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedRisk"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportMixtureClassification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportMixtureProbabilities"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "hazardPlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "hazardTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "includeFullDatasetInSubgroupAnalysis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossDistributions"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossSubgroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergeTablesAcrossMeasures"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "modelSummaryAicWeighs"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "modelSummaryBicWeighs"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettings"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsXAxisFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsXAxisTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsXAxisTransformation"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsYAxisFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsYAxisTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsYAxisTransformation"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeCustom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeRoundSteps"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsNumber"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsSize"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeCustom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsNumber"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsSize"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotGroupUniqueLevels"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotGroupUniqueLevelsLimit"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotPointCoordinates"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotQuantileGroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotStratification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotStratify"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotTiesHandler"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "restrictedMeanSurvivalTimePlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "restrictedMeanSurvivalTimeTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionExponential"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGamma"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedF"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedFOriginal"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedGamma"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedGammaOriginal"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGompertz"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionLogLogistic"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionLogNormal"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionWeibull"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityAsFailureProbability"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotCensoringEvents"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotKaplanMeier"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotTransformXAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotTransformYAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossDistributions"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossSubgroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimePlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeTable"
			condition: function(options) { return options[name] !== undefined; }
		}

	}

	Upgrade
	{
		functionName: "ParametricMixtureSurvivalAnalysis"
		fromVersion: "0.97.1"
		toVersion: "0.97.2"
		msg: qsTr("Prediction, diagnostic and starting-value settings have changed. Please review the updated options and results.")

		// Older Quantiles combined equal-sized and tail partitions. Preserve
		// that grouping only when the saved file has no separate tail setting.
		ChangeJS
		{
			name: "mixtureStartTails"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options)
			{
				if (options["mixtureStartQuantiles"] !== undefined)
					return options["mixtureStartQuantiles"] === true;
				if (options["mixtureStartingValuesQuantiles"] !== undefined)
					return options["mixtureStartingValuesQuantiles"] === true;
				return true;
			}
		}

		ChangeJS
		{
			name: "mixtureStartMembershipProbabilities"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "0.95"; }
		}

		// Rename before initializing defaults; saved values take precedence.

		ChangeRename
		{
			from: "mixtureComponentsTable"
			to: "mixtureMeanAndMedian"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureClassificationTable"
			to: "mixtureClassification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureDiagnosticsTable"
			to: "mixtureEstimationDiagnostics"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotTransformXAxis"
			to: "mixtureComponentPlotXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotTransformYAxis"
			to: "mixtureComponentPlotYAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureStartKmeans"
			to: "mixtureStartingValuesKMeans"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureStartQuantiles"
			to: "mixtureStartingValuesQuantiles"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureStartSplit"
			to: "mixtureStartingValuesSplitOfThePreviousSolution"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureStartRandom"
			to: "mixtureStartingValuesRandom"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureStartRandomCount"
			to: "mixtureStartingValuesRandomCount"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotHistogramBinWidthType"
			to: "densityPlotObservedDataBinWidthType"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotHistogramManualNumberOfBins"
			to: "densityPlotObservedDataNumberOfBins"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotConfidenceInterval"
			to: "densityPlotConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotConfidenceIntervalLevel"
			to: "densityPlotConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotTransformXAxis"
			to: "densityPlotXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotPointCoordinates"
			to: "probabilityPlotEmpiricalPointsCoordinates"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotConfidenceInterval"
			to: "probabilityPlotConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotConfidenceIntervalLevel"
			to: "probabilityPlotConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotTiesHandler"
			to: "probabilityPlotTies"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "coefficientsConfidenceInterval"
			to: "coefficientsConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "coefficientsConfidenceIntervalLevel"
			to: "coefficientsConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "modelSummaryAicWeighs"
			to: "modelSummaryAicWeights"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "modelSummaryBicWeighs"
			to: "modelSummaryBicWeights"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "advancedSpecification"
			to: "modelAdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "advancedSpecificationTerms"
			to: "modelAdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameterRegression"
			to: "modelAncillaryParameterRegression"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettings"
			to: "predictionsCovariateXAxisSettings"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "includeFullDatasetInSubgroupAnalysis"
			to: "outputFormattingIncludeFullDatasetInSubgroupAnalysis"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "compareModelsAcrossDistributions"
			to: "outputFormattingCompareModelsAcrossDistributions"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "compareModelsAcrossComponents"
			to: "outputFormattingCompareModelsAcrossComponents"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "alwaysDisplayModelInformation"
			to: "outputFormattingAlwaysDisplayModelInformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedMean"
			to: "exportFittedValuesMeanSurvivalTime"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedMedian"
			to: "exportFittedValuesMedianSurvivalTime"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedRisk"
			to: "exportFittedValuesRelativeRisk"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedLinearPredictor"
			to: "exportFittedValuesLinearPredictor"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportMixtureProbabilities"
			to: "exportMixtureClassificationComponentProbabilities"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportMixtureClassification"
			to: "exportMixtureClassificationComponentIndex"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotStratify"
			to: "densityPlotStratifyBy"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotStratification"
			to: "densityPlotStratifyByVariables"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotQuantileGroups"
			to: "densityPlotStratifyByQuantiles"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotGroupUniqueLevels"
			to: "densityPlotStratifyByGroupUniqueLevels"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "densityPlotGroupUniqueLevelsLimit"
			to: "densityPlotStratifyByGroupUniqueLevelsLimit"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotStratify"
			to: "probabilityPlotStratifyBy"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotStratification"
			to: "probabilityPlotStratifyByVariables"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotQuantileGroups"
			to: "probabilityPlotStratifyByQuantiles"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotGroupUniqueLevels"
			to: "probabilityPlotStratifyByGroupUniqueLevels"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotGroupUniqueLevelsLimit"
			to: "probabilityPlotStratifyByGroupUniqueLevelsLimit"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotStratify"
			to: "mixtureComponentPlotStratifyBy"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotStratification"
			to: "mixtureComponentPlotStratifyByVariables"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotQuantileGroups"
			to: "mixtureComponentPlotStratifyByQuantiles"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotGroupUniqueLevels"
			to: "mixtureComponentPlotStratifyByGroupUniqueLevels"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "mixtureComponentPlotGroupUniqueLevelsLimit"
			to: "mixtureComponentPlotStratifyByGroupUniqueLevelsLimit"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1ModelTerms"
			to: "modelAncillaryParameter1ModelTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1AdvancedSpecification"
			to: "modelAncillaryParameter1AdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1AdvancedSpecificationTerms"
			to: "modelAncillaryParameter1AdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter1AvailableTerms"
			to: "modelAncillaryParameter1AvailableTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2ModelTerms"
			to: "modelAncillaryParameter2ModelTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2AdvancedSpecification"
			to: "modelAncillaryParameter2AdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2AdvancedSpecificationTerms"
			to: "modelAncillaryParameter2AdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter2AvailableTerms"
			to: "modelAncillaryParameter2AvailableTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3ModelTerms"
			to: "modelAncillaryParameter3ModelTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3AdvancedSpecification"
			to: "modelAncillaryParameter3AdvancedSpecification"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3AdvancedSpecificationTerms"
			to: "modelAncillaryParameter3AdvancedSpecificationTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "ancillaryParameter3AvailableTerms"
			to: "modelAncillaryParameter3AvailableTerms"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsXAxisTransformation"
			to: "predictionsCovariateXAxisSettingsXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsXAxisFrom"
			to: "predictionsCovariateXAxisSettingsXAxisFrom"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsXAxisTo"
			to: "predictionsCovariateXAxisSettingsXAxisTo"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsYAxisTransformation"
			to: "predictionsCovariateXAxisSettingsYAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsYAxisFrom"
			to: "predictionsCovariateXAxisSettingsYAxisFrom"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsCovariateAxisSettingsYAxisTo"
			to: "predictionsCovariateXAxisSettingsYAxisTo"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionExponential"
			to: "selectedParametricDistributionsExponential"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGamma"
			to: "selectedParametricDistributionsGamma"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedF"
			to: "selectedParametricDistributionsGeneralizedF"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGamma"
			to: "selectedParametricDistributionsGeneralizedGamma"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGompertz"
			to: "selectedParametricDistributionsGompertz"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionLogLogistic"
			to: "selectedParametricDistributionsLogLogistic"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionLogNormal"
			to: "selectedParametricDistributionsLogNormal"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionWeibull"
			to: "selectedParametricDistributionsWeibull"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedGammaOriginal"
			to: "selectedParametricDistributionsGeneralizedGammaOriginal"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "selectedParametricDistributionGeneralizedFOriginal"
			to: "selectedParametricDistributionsGeneralizedFOriginal"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsConfidenceInterval"
			to: "predictionsDisplayConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsConfidenceIntervalLevel"
			to: "predictionsDisplayConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotKaplanMeier"
			to: "predictionsDisplayKaplanMeier"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotCensoringEvents"
			to: "predictionsDisplayCensoringEvents"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotTransformXAxis"
			to: "predictionsXAxisTransformation"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "survivalProbabilityPlotTransformYAxis"
			to: "predictionsProbabilityYAxisScale"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "lifeTimeMergeTablesAcrossMeasures"
			to: "predictionsMergeTablesAcrossMeasures"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "predictionsLifeTimeRoundSteps"
			to: "predictionsRoundTimeTableValues"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "probabilityPlotMergePlotsAcrossFactors"
			to: "probabilityPlotMergePlotsAcrossStrata"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeJS
		{
			name: "modelAdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "modelAncillaryParameter1AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAncillaryParameter1AdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "modelAncillaryParameter2AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAncillaryParameter2AdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "modelAncillaryParameter3AdvancedSpecificationTerms"
			isNewOption: true
			condition: function(options) { return upgrades.hasOldModifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
			jsFunction: function(options) { return upgrades.modifierNames(options["modelAncillaryParameter3AdvancedSpecificationTerms"]); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalTime"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "survivalTime"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeSurvivalProbability"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "survivalProbability"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeFailureProbability"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "failureProbability"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeHazard"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "hazard"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeCumulativeHazard"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "cumulativeHazard"); }
		}

		ChangeJS
		{
			name: "predictionsOutcomeRestrictedMeanSurvivalTime"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.outcome(options, "restrictedMeanSurvivalTime"); }
		}

		ChangeJS
		{
			name: "predictionsOutputPlot"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.output(options, "Plot"); }
		}

		ChangeJS
		{
			name: "predictionsOutputTable"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.output(options, "Table"); }
		}

		ChangeJS
		{
			name: "predictionsPredictFor"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "variables"; }
		}

		ChangeJS
		{
			name: "predictionsOutputPlotType"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "standard"; }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentiles"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return options["predictionsOutcomeSurvivalTime"] === true; }
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossDistributions"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.merge(options, "Distributions"); }
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossSubgroups"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.merge(options, "Subgroups"); }
		}

		ChangeJS
		{
			name: "predictionsMergeSeriesAcrossComponents"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.merge(options, "Components"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimeTimeId"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return "\uE000"; }
		}

		ChangeJS
		{
			name: "predictionsXAxisVariables"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return Array.isArray(options["modelTerms"]) ? {value: options["predictionsLifetimeTimeId"], types: "unknown"} : {shouldEncode: true}; }
		}

		ChangeJS
		{
			name: "predictionsSeparateLines"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorAssignments(options, false); }
		}

		ChangeJS
		{
			name: "predictionsSeparatePlots"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorAssignments(options, true); }
		}

		ChangeJS
		{
			name: "predictionsAssignedCovariateValuesQuantiles"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return 3; }
		}

		ChangeJS
		{
			name: "predictionsRolesAutomatic"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return !upgrades.separateFactors(options); }
		}

		// Probability fractions become percentiles; table time counts become intervals.

		ChangeJS
		{
			name: "predictionsLifetimePercentilesGrid"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesGrid"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileGrid(options); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesIntervals"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesIntervals"] === undefined && options["predictionsSurvivalTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsSurvivalTimeStepsNumber"]; }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesFrom"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesFrom"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileSequence(options, "From"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesStep"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesStep"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileSequence(options, "Step"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesTo"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesTo"] === undefined && options["predictionsSurvivalTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileSequence(options, "To"); }
		}

		ChangeJS
		{
			name: "predictionsLifetimePercentilesValues"
			isNewOption: true
			condition: function(options) { return options["predictionsLifetimePercentilesValues"] === undefined && options["predictionsSurvivalTimeCustom"] !== undefined; }
			jsFunction: function(options) { return upgrades.percentileValues(options); }
		}

		ChangeJS
		{
			name: "predictionsTimeGrid"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeGrid"] === undefined && options["predictionsLifeTimeStepsType"] !== undefined; }
			jsFunction: function(options) { return upgrades.timeGrid(options); }
		}

		ChangeJS
		{
			name: "predictionsTimeGrid"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeGrid"] === "custom"; }
			jsFunction: function(options) { return "customAbsolute"; }
		}

		ChangeJS
		{
			name: "predictionsTimeIntervals"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeIntervals"] === undefined && options["predictionsLifeTimeStepsNumber"] !== undefined; }
			jsFunction: function(options) { return options["predictionsLifeTimeStepsNumber"] - 1; }
		}

		ChangeJS
		{
			name: "predictionsTimeFrom"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeFrom"] === undefined && options["predictionsLifeTimeStepsFrom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsFrom"]); }
		}

		ChangeJS
		{
			name: "predictionsTimeStep"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeStep"] === undefined && options["predictionsLifeTimeStepsSize"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsSize"]); }
		}

		ChangeJS
		{
			name: "predictionsTimeTo"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeTo"] === undefined && options["predictionsLifeTimeStepsTo"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeStepsTo"]); }
		}

		ChangeJS
		{
			name: "predictionsTimeValues"
			isNewOption: true
			condition: function(options) { return options["predictionsTimeValues"] === undefined && options["predictionsLifeTimeCustom"] !== undefined; }
			jsFunction: function(options) { return String(options["predictionsLifeTimeCustom"]); }
		}

		ChangeJS
		{
			name: "probabilityPlotStratifyBy"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return true; }
		}

		ChangeJS
		{
			name: "probabilityPlotStratifyByVariables"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorStrata(options); }
		}

		// Observed histograms now belong to the separate density diagnostic.

		ChangeJS
		{
			name: "densityPlot"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.observedDensity(options); }
		}

		ChangeJS
		{
			name: "densityPlotObservedData"
			isNewOption: true
			condition: function(options) { return options["densityPlotObservedData"] === undefined && options["mixtureComponentPlotObservedData"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotObservedData"] === true; }
		}

		ChangeJS
		{
			name: "densityPlotXAxisTransformation"
			isNewOption: true
			condition: function(options) { return options["densityPlotXAxisTransformation"] === undefined && options["mixtureComponentPlotXAxisTransformation"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotXAxisTransformation"]; }
		}

		ChangeJS
		{
			name: "densityPlotMergePlotsAcrossDistributions"
			isNewOption: true
			condition: function(options) { return options["densityPlotMergePlotsAcrossDistributions"] === undefined && options["mixtureComponentPlotMergePlotsAcrossDistributions"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotMergePlotsAcrossDistributions"] === true; }
		}

		ChangeJS
		{
			name: "densityPlotConfidenceIntervals"
			isNewOption: true
			condition: function(options) { return options["densityPlotConfidenceIntervals"] === undefined && options["predictionsDisplayConfidenceIntervals"] !== undefined; }
			jsFunction: function(options) { return options["predictionsDisplayConfidenceIntervals"] === true; }
		}

		ChangeJS
		{
			name: "densityPlotConfidenceIntervalsLevel"
			isNewOption: true
			condition: function(options) { return options["densityPlotConfidenceIntervalsLevel"] === undefined && options["predictionsDisplayConfidenceIntervalsLevel"] !== undefined; }
			jsFunction: function(options) { return options["predictionsDisplayConfidenceIntervalsLevel"]; }
		}

		ChangeJS
		{
			name: "densityPlotObservedDataBinWidthType"
			isNewOption: true
			condition: function(options) { return options["densityPlotObservedDataBinWidthType"] === undefined && options["mixtureComponentPlotHistogramBinWidthType"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotHistogramBinWidthType"]; }
		}

		ChangeJS
		{
			name: "densityPlotObservedDataNumberOfBins"
			isNewOption: true
			condition: function(options) { return options["densityPlotObservedDataNumberOfBins"] === undefined && options["mixtureComponentPlotHistogramManualNumberOfBins"] !== undefined; }
			jsFunction: function(options) { return options["mixtureComponentPlotHistogramManualNumberOfBins"]; }
		}

		ChangeJS
		{
			name: "densityPlotStratifyBy"
			isNewOption: true
			condition: function(options) { return options["densityPlotStratifyBy"] === undefined && options["mixtureComponentPlotObservedData"] !== undefined; }
			jsFunction: function(options) { return true; }
		}

		ChangeJS
		{
			name: "densityPlotStratifyByVariables"
			isNewOption: true
			condition: function(options) { return options["densityPlotStratifyByVariables"] === undefined && options["mixtureComponentPlotObservedData"] !== undefined; }
			jsFunction: function(options) { return upgrades.factorStrata(options); }
		}

		ChangeJS
		{
			name: "mixtureComponentPlotStratifyBy"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return true; }
		}

		ChangeJS
		{
			name: "mixtureComponentPlotStratifyByVariables"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
			jsFunction: function(options) { return upgrades.factorStrata(options); }
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

		ChangeJS
		{
			name: "modelAncillaryParameter1ModelTerms"
			isNewOption: true
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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
			condition: function(options) { return options[name] === undefined; }
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

		ChangeRemove
		{
			name: "advancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "advancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "alwaysDisplayModelInformation"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1AdvancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1AdvancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1AvailableTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter1ModelTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2AdvancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2AdvancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2AvailableTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter2ModelTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3AdvancedSpecification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3AdvancedSpecificationTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3AvailableTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameter3ModelTerms"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "ancillaryParameterRegression"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "coefficientsConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "coefficientsConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "compareModelsAcrossComponents"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "compareModelsAcrossDistributions"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "cumulativeHazardPlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "cumulativeHazardTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotGroupUniqueLevels"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotGroupUniqueLevelsLimit"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotHistogramBinWidthType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotHistogramManualNumberOfBins"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotQuantileGroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotStratification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotStratify"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "densityPlotTransformXAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedLinearPredictor"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedMean"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedMedian"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedRisk"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportMixtureClassification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportMixtureProbabilities"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "hazardPlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "hazardTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "includeFullDatasetInSubgroupAnalysis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossComponents"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossDistributions"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergePlotsAcrossSubgroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "lifeTimeMergeTablesAcrossMeasures"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureClassificationTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotGroupUniqueLevels"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotGroupUniqueLevelsLimit"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotHistogramBinWidthType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotHistogramManualNumberOfBins"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotObservedData"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotQuantileGroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotStratification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotStratify"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotTransformXAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentPlotTransformYAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureComponentsTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureDiagnosticsTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureStartKmeans"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureStartQuantiles"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureStartRandom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureStartRandomCount"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "mixtureStartSplit"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "modelSummaryAicWeighs"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "modelSummaryBicWeighs"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettings"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsXAxisFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsXAxisTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsXAxisTransformation"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsYAxisFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsYAxisTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsCovariateAxisSettingsYAxisTransformation"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeCustom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeRoundSteps"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsNumber"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsSize"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsLifeTimeStepsType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeCustom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsFrom"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsNumber"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsSize"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsTo"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "predictionsSurvivalTimeStepsType"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotGroupUniqueLevels"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotGroupUniqueLevelsLimit"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotPointCoordinates"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotQuantileGroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotStratification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotStratify"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "probabilityPlotTiesHandler"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "restrictedMeanSurvivalTimePlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "restrictedMeanSurvivalTimeTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionExponential"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGamma"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedF"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedFOriginal"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedGamma"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGeneralizedGammaOriginal"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionGompertz"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionLogLogistic"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionLogNormal"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "selectedParametricDistributionWeibull"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityAsFailureProbability"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotCensoringEvents"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotKaplanMeier"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotTransformXAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityPlotTransformYAxis"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalProbabilityTable"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossComponents"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossDistributions"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossFactors"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeMergePlotsAcrossSubgroups"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimePlot"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "survivalTimeTable"
			condition: function(options) { return options[name] !== undefined; }
		}

	}

	Upgrade
	{
		functionName: "SemiParametricSurvivalAnalysis"
		fromVersion: "0.97.1"
		toVersion: "0.97.2"

		// Rename before initializing defaults; saved values take precedence.

		ChangeRename
		{
			from: "coefficientsConfidenceInterval"
			to: "coefficientsConfidenceIntervals"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "coefficientsConfidenceIntervalLevel"
			to: "coefficientsConfidenceIntervalsLevel"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedMean"
			to: "exportFittedValuesMeanSurvivalTime"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedMedian"
			to: "exportFittedValuesMedianSurvivalTime"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedRisk"
			to: "exportFittedValuesRelativeRisk"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportFittedLinearPredictor"
			to: "exportFittedValuesLinearPredictor"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportMixtureProbabilities"
			to: "exportMixtureClassificationComponentProbabilities"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		ChangeRename
		{
			from: "exportMixtureClassification"
			to: "exportMixtureClassificationComponentIndex"
			condition: function(options) { return options[from] !== undefined && options[to] === undefined; }
		}

		// Consume old options only after every dependent setting has been converted.

		ChangeRemove
		{
			name: "coefficientsConfidenceInterval"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "coefficientsConfidenceIntervalLevel"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedLinearPredictor"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedMean"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedMedian"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportFittedRisk"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportMixtureClassification"
			condition: function(options) { return options[name] !== undefined; }
		}

		ChangeRemove
		{
			name: "exportMixtureProbabilities"
			condition: function(options) { return options[name] !== undefined; }
		}

	}
}
