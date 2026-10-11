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
