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

Form
{
	info: qsTr("This analysis performs a parametric survival analysis.")

	VariablesForm
	{
		removeInvisibles:	true
		preferredHeight:	((censoringTypeRight.checked  || censoringTypeLeft.checked) ? 450 : 500 ) * jaspTheme.uiScale

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
			visible:			censoringTypeInterval.checked
			property bool active:	censoringTypeInterval.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Interval.")
		}

		AssignedVariablesList
		{
			name:				"intervalEnd"
			title:				qsTr("Interval End")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeInterval.checked
			property bool active:	censoringTypeInterval.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Interval.")
		}

		AssignedVariablesList
		{
			name:				"timeToEvent"
			title:				qsTr("Time to Event")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeRight.checked || censoringTypeLeft.checked
			property bool active:	censoringTypeRight.checked || censoringTypeLeft.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right or Left.")
		}

		AssignedVariablesList
		{
			id:					eventStatusId
			name:				"eventStatus"
			title:				qsTr("Event Status")
			allowedColumns:		["nominal"]
			singleVariable:		true
			info: qsTr("Choose the variable that indicates the event status, specifying whether each observation is an event or censored.")
		}

		DropDown
		{
			name:				"eventIndicator"
			label:				qsTr("Event Indicator")
			source:				[{name: "eventStatus", use: "levels"}]
			onCountChanged:		currentIndex = 1
			info: qsTr("Specify the value in the Event Status variable that indicates the occurrence of the event.")
		}

		AssignedVariablesList
		{
			name:			 	"covariates"
			title:			 	qsTr("Covariates")
			allowedColumns:		["scale"]
			info: qsTr("Add continuous variables as covariates to include them in the Cox regression model.")
		}

		AssignedVariablesList
		{
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["nominal"]
			info: qsTr("Add categorical variables as factors to include them in the Cox regression model.")
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
			allowedColumns:		["nomial"]
			singleVariable:		true
			info: qsTr("Select a variable for subgroup analysis, allowing for separate analyses within each subgroup.")
		}
	}

	RadioButtonGroup
	{
		id:						censoringType
		Layout.columnSpan:		1
		name:					"censoringType"
		title:					qsTr("Censoring Type")
		radioButtonsOnSameRow:	true
		columns:				3
		info: qsTr("Select the type of censoring in your data: right, left, or interval censoring.")

		RadioButton
		{
			label:		qsTr("Right")
			value:		"right"
			id:			censoringTypeRight
			checked:	true
		}

		RadioButton
		{
			label:		qsTr("Left")
			value:		"left"
			id:			censoringTypeLeft
		}

		RadioButton
		{
			label:		qsTr("Interval")
			value:		"interval"
			id:			censoringTypeInterval
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

		CheckBox
		{
			name:		"includeIntercept"
			label:		qsTr("Include intercept")
			checked:	true
		}

		DropDown
		{
			name:				"interpretModel"
			label:				qsTr("Interpret model")
			enabled:			modelTerms.count > 1
			onCountChanged:		if (!(value === "bestAic" || value === "bestBic" || value === "all")) currentIndex = count - 1
			info: qsTr("Select the model to interpret. Defaults to the last specified model. Alternatives are 'All' which produces results for all of the specified models or 'Best' which produces results for the best fitting model based on either AIC or BIC.")
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

	Section
	{
		title: qsTr("Statistics")

		CheckBox
		{
			label:		qsTr("Model summary")
			name:		"modelSummary"
			checked:	true
			info: qsTr("Include a table with information about the model fit.")

			CheckBox 
			{
				name:		"modelSummaryRankModels"
				label:		qsTr("Rank models")
				enabled:	distribution.value == "all" || modelTerms.count > 1
				info: qsTr("Rank models based on the selected criterion.")

				RadioButtonGroup
				{
					name:		"modelSummaryRankModelsBy"

					RadioButton
					{
						label:		qsTr("AIC")
						value:		"aic"
						checked:	true
					}

					RadioButton
					{
						label:		qsTr("BIC")
						value:		"bic"
					}

					RadioButton
					{
						label:		qsTr("Log lik.")
						value:		"logLik"
					}
				}
			}

			CheckBox
			{
				name:		"modelSummaryAicWeighs"
				label:		qsTr("AIC weights")
				enabled:	distribution.value == "all" || modelTerms.count > 1
				info: qsTr("Include AIC weights in the model summary.")
			}

			CheckBox
			{
				name:		"modelSummaryBicWeighs"
				label:		qsTr("BIC weights")
				enabled:	distribution.value == "all" || modelTerms.count > 1
				info: qsTr("Include BIC weights in the model summary.")
			}
		}

		Group
		{
			CheckBox
			{
				label:		qsTr("Coefficients")
				name:		"coefficients"
				checked:	false
				info: qsTr("Include a table with coefficients estimates.")
			}

			CheckBox
			{
				label:		qsTr("Coefficients covariance matrix")
				name:		"coefficientsCovarianceMatrix"
				checked:	false
				info: qsTr("Include a table with the covariance matrix of the coefficients estimates.")
			}
		}

	}

	Section
	{
		title: qsTr("Advanced")

		Group
		{
			title:		qsTr("Selected Parametric Distributions")
			enabled:	distribution.value == "all" || distribution.value == "bestAIC" || distribution.value == "bestBIC"


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
				enabled:	(distribution.value == "all" || distribution.value == "bestAic" || distribution.value == "bestBic") && modelTerms.count > 1
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
	}
}
