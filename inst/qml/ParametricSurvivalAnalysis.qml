//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP
import "./qml_components" as SA

Form
{
	info: qsTr("This analysis performs a parametric survival analysis.")

	property bool	categoricalPredictionLevelsPossible:		factors.count > 0 && modelTerms.countVariables > 0

	VariablesForm
	{
		removeInvisibles:	true
		preferredHeight:	((censoringTypeRight.checked  || censoringTypeInterval.checked) ? 450 : 525 ) * jaspTheme.uiScale

		AvailableVariablesList
		{
			name: "allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"intervalStart"
			title:				qsTr("Interval Start")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeInterval.checked || censoringTypeCounting.checked
			property bool active:	censoringTypeInterval.checked  || censoringTypeCounting.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Interval or Counting.")
		}

		AssignedVariablesList
		{
			name:				"intervalEnd"
			title:				qsTr("Interval End")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeInterval.checked || censoringTypeCounting.checked
			property bool active:	censoringTypeInterval.checked || censoringTypeCounting.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Interval or Counting.")
		}

		AssignedVariablesList
		{
			name:				"timeToEvent"
			title:				qsTr("Time to Event")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeRight.checked
			property bool active:	censoringTypeRight.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right.")
		}

		AssignedVariablesList
		{
			id:					eventStatusId
			name:				"eventStatus"
			title:				qsTr("Event Status")
			visible:			censoringTypeRight.checked || censoringTypeCounting.checked
			property bool active:	censoringTypeRight.checked || censoringTypeCounting.checked
			allowedColumns:		["nominal"]
			singleVariable:		true
			info: qsTr("Choose the variable that indicates the event status, specifying whether each observation is an event or censored.")
		}

		DropDown
		{
			name:				"eventIndicator"
			label:				qsTr("Event Indicator")
			visible:			censoringTypeRight.checked || censoringTypeCounting.checked
			property bool active:	censoringTypeRight.checked || censoringTypeCounting.checked
			source:				[{name: "eventStatus", use: "levels"}]
			onCountChanged:		currentIndex = 1
			info: qsTr("Specify the value in the Event Status variable that indicates the occurrence of the event.")
		}

		AssignedVariablesList
		{
			id:				 	covariates
			name:			 	"covariates"
			title:			 	qsTr("Covariates")
			allowedColumns:		["scale"]
			info: qsTr("Add continuous variables as covariates to include them in the parametric survival model.")
		}

		AssignedVariablesList
		{
			id:				 	factors
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["nominal"]
			info: qsTr("Add categorical variables as factors to include them in the parametric survival model.")
		}


		AssignedVariablesList
		{
			name:			 	"weights"
			title:			 	qsTr("Weights")
			allowedColumns:		["scale"]
			singleVariable:		true
			info: qsTr("Select a variable for case weights, weighting each observation accordingly in the model.")
		}

		AssignedVariablesList
		{
			name:			 	"subgroup"
			id:					subgroup
			title:			 	qsTr("Subgroup")
			allowedColumns:		["nominal"]
			singleVariable:		true
			info: qsTr("Select a variable for subgroup analysis, allowing for separate analyses within each subgroup.")
		}
	}

	Group
	{
		RadioButtonGroup
		{
			id:						censoringType
			Layout.columnSpan:		1
			name:					"censoringType"
			title:					qsTr("Censoring Type")
			radioButtonsOnSameRow:	true
			columns:				3
			info: qsTr("Select right-censored data, counting-process data with delayed entry, or interval-censored data.")

			RadioButton
			{
				label:		qsTr("Right")
				value:		"right"
				id:			censoringTypeRight
				checked:	true
			}

			RadioButton
			{
				label:		qsTr("Counting")
				value:		"counting"
				id:			censoringTypeCounting
			}

			RadioButton
			{
				label:		qsTr("Interval")
				value:		"interval"
				id:			censoringTypeInterval
				info: qsTr("If interval censoring is selected, the following coding needs to be used: left-censored data is represented as (NA, t2), right-censored data as (t1, NA), exact data as (t, t), and interval-censored data as (t1, t2).")
			}
		}

		CheckBox
		{
			name:		"censoringSummary"
			label:		qsTr("Censoring summary")
			info: qsTr("Create a summary table with information about the censoring status of the data.")
		}
	}

	DropDown
	{
		name:		"distribution"
		id:			distribution
		label:		qsTr("Distribution")
		startValue:	"weibull"
		info: qsTr("Choose the parametric distribution for the analysis. All fits and display results for all 'Selected parametric families' in the 'Advanced' section. 'Best AIC' and 'Best BIC' fit all `Selected parametric families` in the Advanced section and display the results only for a parametric family with the lowest AIC/BIC.")
		values:
		[
			{ label: qsTr("Exponential"),						value: "exponential" },
			{ label: qsTr("Gamma"),								value: "gamma" },
			{ label: qsTr("Generalized F"),						value: "generalizedF" },
			{ label: qsTr("Generalized gamma"),					value: "generalizedGamma" },
			{ label: qsTr("Gompertz"),							value: "gompertz" },
			{ label: qsTr("Log-logistic"),						value: "logLogistic" },
			{ label: qsTr("Log-normal"),						value: "logNormal" },
			{ label: qsTr("Weibull"),							value: "weibull" },
			{ label: qsTr("Generalized gamma (original)"),		value: "generalizedGammaOriginal" },
			{ label: qsTr("Generalized F (original)"),			value: "generalizedFOriginal" },
			{ label: qsTr("All"),								value: "all"},
			{ label: qsTr("Best AIC"),							value: "bestAic"},
			{ label: qsTr("Best BIC"),							value: "bestBic"}
		]
	}

	Section
	{
		title: qsTr("Model")

		FactorsForm
		{
			name:				"modelTerms"
			id:					modelTerms
			nested:				true
			startIndex:			1
			initNumberFactors:	1
			allowInteraction:	true
			baseName:			"model"
			baseTitle:			qsTr("Model")
			availableVariablesListName:		"availableTerms"
			availableVariablesList.source:	['covariates', 'factors']
			allowedColumns:		[]
		}

		DropDown
		{
			id:					interpretModel
			name:				"interpretModel"
			label:				qsTr("Interpret model")
			enabled:			modelTerms.count > 1
			onCountChanged:		if (!(value === "bestAic" || value === "bestBic" || value === "all")) currentIndex = count - 1
			info: qsTr("Select the model to interpret. Defaults to the last specified model. Alternatives are 'All' which produces results for all of the specified models or 'Best' which produces results for the best fitting model based on either AIC or BIC. If distribution and model selection is specified simultanously, the best model within the best performing distribution is going to be selected. If model selection is specified while all distributions are selected, the best model within each distribution is going to be selected.")
			startValue:			"model1"
			source:
			[
				{
					values: [
						{label: qsTr("All"),		value: "all"},
						{label: qsTr("Best AIC"),	value: "bestAic"},
						{label: qsTr("Best BIC"),	value: "bestBic"}
					]
				},
				{
					values: modelTerms.factorsTitles
				}
			]
		}
	}

	SA.ParametricStatistics
	{
		multipleModels:	modelTerms.count > 1
		multipleResults:	distribution.value === "all" || modelTerms.count > 1
	}

	SA.ParametricPredictions
	{
		rightCensoring:	censoringTypeRight.checked
		mergeDistributionsAvailable:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
		categoricalLevelsPossible:	categoricalPredictionLevelsPossible
	}

	Section
	{
		title:	qsTr("Diagnostics")

		SA.ParametricResidualPlots
		{
			rightCensoring:	censoringTypeRight.checked
		}

		SA.ParametricProbabilityPlot
		{
			rightCensoring:	censoringTypeRight.checked
			mergeDistributionsAvailable:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
			categoricalLevelsPossible:	categoricalPredictionLevelsPossible
		}
	}

	SA.SurvivalExport
	{
		intervalCensoring: censoringTypeInterval.checked
	}

	Section
	{
		title: qsTr("Advanced")

		Group
		{
			title:		qsTr("Selected Parametric Distributions")
			enabled:	distribution.value === "all" || distribution.value === "bestAic" || distribution.value === "bestBic"


			CheckBox { name: "selectedParametricDistributionExponential";				label: qsTr("Exponential");						checked: true }
			CheckBox { name: "selectedParametricDistributionGamma";						label: qsTr("Gamma");							checked: true }
			CheckBox { name: "selectedParametricDistributionGeneralizedF";				label: qsTr("Generalized F");					checked: true }
			CheckBox { name: "selectedParametricDistributionGeneralizedGamma";			label: qsTr("Generalized gamma");				checked: true }
			CheckBox { name: "selectedParametricDistributionGompertz";					label: qsTr("Gompertz");						checked: true }
			CheckBox { name: "selectedParametricDistributionLogLogistic";				label: qsTr("Log-logistic");					checked: true }
			CheckBox { name: "selectedParametricDistributionLogNormal";					label: qsTr("Log-normal");						checked: true }
			CheckBox { name: "selectedParametricDistributionWeibull";					label: qsTr("Weibull");							checked: true }
			CheckBox { name: "selectedParametricDistributionGeneralizedGammaOriginal"; 	label: qsTr("Generalized gamma (original)");	checked: false }
			CheckBox { name: "selectedParametricDistributionGeneralizedFOriginal"; 		label: qsTr("Generalized F (original)");		checked: false }
		}

		Group
		{
			title:		qsTr("Output Formatting")

			CheckBox
			{
				name:		"includeFullDatasetInSubgroupAnalysis"
				text:		qsTr("Include full dataset in subgroup analysis")
				enabled:	subgroup.count == 1
				checked:	false
				info: qsTr("Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.")
			}

			CheckBox
			{
				name:		"compareModelsAcrossDistributions"
				text:		qsTr("Compare models across distributions")
				enabled:	(distribution.value === "all" || distribution.value === "bestAic" || distribution.value === "bestBic") && modelTerms.count > 1
				checked:	true
				info: qsTr("Compare models across distributions. This option is only available when the multiple models and parametric distributions are specified.")
			}

			CheckBox
			{
				name:		"alwaysDisplayModelInformation"
				text:		qsTr("Always display model information")
				checked:	false
				info: qsTr("Always display model information (distribution and model name) in output tables.")
			}
		}

		Group
		{
			IntegerField
			{
				name:			"confidenceIntervalSimulationDraws"
				label:			qsTr("Confidence interval simulation draws")
				defaultValue:	10000
				min:			100
				max:			1000000
				info: qsTr("Set the number of parameter draws used to simulate confidence intervals for prediction tables and all model-based plot bands. More draws improve precision at a higher computational cost.")
			}

			SetSeed {}
		}
	}
}
