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
			info: qsTr("Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right or Counting.")
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
				enabled:	distribution.value === "all" || modelTerms.count > 1
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
				enabled:	distribution.value === "all" || modelTerms.count > 1
				info: qsTr("Include AIC weights in the model summary.")
			}

			CheckBox
			{
				name:		"modelSummaryBicWeighs"
				label:		qsTr("BIC weights")
				enabled:	distribution.value === "all" || modelTerms.count > 1
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
				info: qsTr("Include a table with coefficient estimates.")

				CIField
				{
					name: "coefficientsConfidenceIntervalLevel"
					label: qsTr("Confidence interval")
				}
			}

			CheckBox
			{
				label:		qsTr("Coefficients covariance matrix")
				name:		"coefficientsCovarianceMatrix"
				checked:	false
				info: qsTr("Include a table with the covariance matrix of the coefficient estimates.")
			}
		}

		CheckBox
		{
			label:		qsTr("Sequential model comparison")
			name:		"sequentialModelComparison"
			checked:	false
			enabled:	modelTerms.count > 1
			info: qsTr("Include a table with the results of the sequential model comparison.")
		}

	}

	Section
	{
		title: qsTr("Predictions")

		Group
		{

			Group
			{
				title:		qsTr("Survival Time")

				CheckBox
				{
					label:		qsTr("Table")
					name:		"survivalTimeTable"
					info: qsTr("Include a table with the predicted survival estimates.")
				}

				CheckBox
				{
					label:		qsTr("Plot")
					name:		"survivalTimePlot"
					info: qsTr("Include a plot with the predicted survival estimates.")
				}
			}

			DropDown
			{
				name:		"predictionsSurvivalTimeStepsType"
				id:			predictionsSurvivalTimeStepsType
				label:		qsTr("Steps type")
				info: qsTr("Select the method to define intervals for the life table: Default, Quantiles, or Custom.")
				values:
				[
					{ label: qsTr("Quantilies"),	value: "quantiles"},
					{ label: qsTr("Sequence"),		value: "sequence"},
					{ label: qsTr("Custom"),		value: "custom"}
				]
			}

			IntegerField
			{
				name:			"predictionsSurvivalTimeStepsNumber"
				label:			qsTr("Number")
				defaultValue:	10
				min:			2
				visible:		predictionsSurvivalTimeStepsType.value === "quantiles"
				info: qsTr("Specify the number of quantiles of the predicted surival when using Quantiles as the steps type.")
			}

			DoubleField
			{
				name:			"predictionsSurvivalTimeStepsFrom"
				id:				predictionsSurvivalTimeStepsFrom
				label:			qsTr("From")
				defaultValue:	0
				min:			0
				max:			predictionsSurvivalTimeStepsTo.value
				visible:		predictionsSurvivalTimeStepsType.value === "sequence"
				info: qsTr("Set the starting quantile of the predicted survival when using Sequence steps.")
			}

			DoubleField
			{
				name:			"predictionsSurvivalTimeStepsSize"
				id:				predictionsSurvivalTimeStepsSize
				label:			qsTr("Size")
				defaultValue:	0.1
				max:			1
				visible:		predictionsSurvivalTimeStepsType.value === "sequence"
				info: qsTr("Define the size of each quantile of the predicted survival when using Sequence steps.")
			}

			DoubleField
			{
				name:			"predictionsSurvivalTimeStepsTo"
				id:				predictionsSurvivalTimeStepsTo
				label:			qsTr("To")
				defaultValue:	1
				min:			predictionsSurvivalTimeStepsFrom.value + predictionsSurvivalTimeStepsSize.value
				max:			1
				visible:		predictionsSurvivalTimeStepsType.value === "sequence"
				info: qsTr("Set the ending quantile of the predicted survival when using Sequence steps.")
			}

			FormulaField
			{
				name:			"predictionsSurvivalTimeCustom"
				label:			qsTr("Steps")
				visible:		predictionsSurvivalTimeStepsType.value === "custom"
				defaultValue:	"0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9"
				info: qsTr("Specify custom steps of the predicted survival.")
			}

			CheckBox
			{
				label:		qsTr("Merge plots across distributions")
				name:		"survivalTimeMergePlotsAcrossDistributions"
				checked:	false
				enabled:	distribution.value === "all" && (modelTerms.count  == 1 || (modelTerms.count > 1 && interpretModel.value != "bestAic" && interpretModel.value != "bestBic"))
				info: qsTr("Merge the plots for survival probability across distributions into a single plot. Only available when no model selection is being performed.")
			}


			Group
			{
				title:		qsTr("Options")

				CheckBox
				{
					label:		qsTr("Confidence intervals")
					name:		"predictionsConfidenceInterval"
					checked:	true
					info: qsTr("Include confidence intervals for the figures and tables.")

					CIField
					{
						name: "predictionsConfidenceIntervalLevel"
						label: qsTr("Confidence interval")
					}
				}

				DropDown
				{
					name:		"plotLegend"
					label:		qsTr("Legend")
					startValue:	"right"
					info: qsTr("Choose the position of the legend on the plot: Bottom, Right, Left, Top, or None. Only available when predictors or multiple distributions are specified.")
					values:
					[
						{ label: qsTr("Bottom"),	value: "bottom"},
						{ label: qsTr("Right"),		value: "right"},
						{ label: qsTr("Left"),		value: "left"},
						{ label: qsTr("Top"),		value: "top"},
						{ label: qsTr("None"),		value: "none"}
					]
				}

				ColorPalette {}

				DropDown
				{
					name:		"plotTheme"
					label:		qsTr("Theme")
					startValue:	"jasp"
					info: qsTr("Select the theme for the plot's appearance. The detailed theme works only for 'Survival probabilities' plots")
					values:
					[
						{ label: "JASP",					value: "jasp"},
						{ label: qsTr("White background"),	value: "whiteBackground"},
						{ label: qsTr("Light"),				value: "light"},
						{ label: qsTr("Minimal")	,		value: "minimal"},
						{ label: "APA",						value: "apa"},
						{ label: "pubr",					value: "pubr"},
						{ label: qsTr("Detailed"),			value: "detailed"}
					]
				}
			}
		}

		Group
		{
			Group
			{
				title:		qsTr("Survival Probability")

				CheckBox
				{
					label:		qsTr("Table")
					name:		"survivalProbabilityTable"
					info: qsTr("Include a table with the predicted survival probabilities.")
				}

				CheckBox
				{
					label:		qsTr("Plot")
					name:		"survivalProbabilityPlot"
					info: qsTr("Include a plot with the predicted survival probabilities.")

					CheckBox
					{
						name:		"survivalProbabilityPlotAddKaplanMeier"
						label:		qsTr("Add Kaplan-Meier")
						enabled:	censoringTypeRight.checked
						info: qsTr("Add a Kaplan-Meier curve to the plot. Only available when Censoring Type is set to Right.")
					}

					CheckBox
					{
						name:		"survivalProbabilityPlotAddCensoringEvents"
						label:		qsTr("Add censoring events")
						enabled:	censoringTypeRight.checked
						info: qsTr("Add censoring events as rug marks to the plot. Only available when Censoring Type is set to Right.")
					}

					DropDown
					{
						name:		"survivalProbabilityPlotTransformXAxis"
						label:		qsTr("X-axis transformation")
						startValue:	"none"
						info: qsTr("Select the transformation for the x-axis of the plot")
						values:
						[
							{ label: qsTr("None"),			value: "none"},
							{ label: qsTr("Log"),			value: "log"}
						]
					}

					DropDown
					{
						name:		"survivalProbabilityPlotTransformYAxis"
						label:		qsTr("Y-axis transformation")
						startValue:	"none"
						info: qsTr("Select the transformation for the y-axis of the plot")
						values:
						[
							{ label: qsTr("None"),				value: "none"},
							{ label: qsTr("Log"),				value: "log"},
							{ label: qsTr("Log(-log(1-p))"),	value: "logmlogmp"}
						]
					}
				}

				CheckBox
				{
					label:		qsTr("As failure probability")
					name:		"survivalProbabilityAsFailureProbability"
					info: qsTr("Transform the output from survival probability to failure probability (i.e., 1 - Survival Probability).")
				}
			}

			Group
			{
				title: qsTr("Hazard")

				CheckBox
				{
					label: qsTr("Table")
					name: "hazardTable"
					info: qsTr("Include a table with the predicted hazard estimates.")
				}

				CheckBox
				{
					label: qsTr("Plot")
					name: "hazardPlot"
					info: qsTr("Include a plot with the predicted hazard estimates.")
				}
			}

			Group
			{
				title: qsTr("Cumulative Hazard")

				CheckBox
				{
					label: qsTr("Table")
					name: "cumulativeHazardTable"
					info: qsTr("Include a table with the predicted cumulative hazard estimates.")
				}

				CheckBox
				{
					label: qsTr("Plot")
					name: "cumulativeHazardPlot"
					info: qsTr("Include a plot with the predicted cumulative hazard estimates.")
				}
			}

			Group
			{
				title: qsTr("Restricted Mean Survival Time")

				CheckBox
				{
					label: qsTr("Table")
					name: "restrictedMeanSurvivalTimeTable"
					info: qsTr("Include a table with the restricted mean survival time estimates.")
				}

				CheckBox
				{
					label: qsTr("Plot")
					name: "restrictedMeanSurvivalTimePlot"
					info: qsTr("Include a plot with the restricted mean survival time estimates.")
				}
			}

			DropDown
			{
				name:		"predictionsLifeTimeStepsType"
				id:			predictionsLifeTimeStepsType
				label:		qsTr("Steps type")
				info: qsTr("Select the method to define intervals for the life table: Default, Quantiles, or Custom.")
				values:
				[
					{ label: qsTr("Quantilies"),	value: "quantiles"},
					{ label: qsTr("Sequence"),		value: "sequence"},
					{ label: qsTr("Custom"),		value: "custom"}
				]
			}

			IntegerField
			{
				name:			"predictionsLifeTimeStepsNumber"
				label:			qsTr("Number")
				defaultValue:	10
				min:			2
				visible:		predictionsLifeTimeStepsType.value === "quantiles"
				info: qsTr("Specify the number of quantiles of the life time when using Quantiles as the steps type.")
			}

			FormulaField
			{
				name:			"predictionsLifeTimeStepsFrom"
				id:				predictionsLifeTimeStepsFrom
				label:			qsTr("From")
				defaultValue:	0
				min:			0
				max:			predictionsLifeTimeStepsTo.value
				visible:		predictionsLifeTimeStepsType.value === "sequence"
				fieldWidth:		40 * jaspTheme.uiScale
				info: qsTr("Set the starting quantile of the life time when using Sequence steps.")
			}

			FormulaField
			{
				name:			"predictionsLifeTimeStepsSize"
				id:				predictionsLifeTimeStepsSize
				label:			qsTr("Size")
				defaultValue:	""
				visible:		predictionsLifeTimeStepsType.value === "sequence"
				fieldWidth:		40 * jaspTheme.uiScale
				info: qsTr("Define the size of each quantile of the life time when using Sequence steps. The default '' corresponds to 1/10 of the maximum observed time.")
			}

			FormulaField
			{
				name:			"predictionsLifeTimeStepsTo"
				id:				predictionsLifeTimeStepsTo
				label:			qsTr("To")
				min:			predictionsLifeTimeStepsFrom.value
				defaultValue:	""
				visible:		predictionsLifeTimeStepsType.value === "sequence"
				fieldWidth:		40 * jaspTheme.uiScale
				info: qsTr("Set the final step of the life time when using Sequence steps. The default '' corresponds to the maximum observed time.")
			}

			CheckBox
			{
				name:		"predictionsLifeTimeRoundSteps"
				label:		qsTr("Round steps")
				checked:	true
				visible:	predictionsLifeTimeStepsType.value === "quantiles" || predictionsLifeTimeStepsType.value === "sequence"
				info: qsTr("Round the interval boundaries to the nearest integer when using Quantiles or Sequence steps.")
			}

			FormulaField
			{
				name:			"predictionsLifeTimeCustom"
				label:			qsTr("Steps")
				visible:		predictionsLifeTimeStepsType.value === "custom"
				defaultValue:	"0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9"
				info: qsTr("Specify custom steps of the life time.")
			}

			CheckBox
			{
				label:		qsTr("Merge tables across measures")
				name:		"lifeTimeMergeTablesAcrossMeasures"
				checked:	false
				info: qsTr("Merge the tables for survival time, survival probabilities, hazard, cumulative hazard, and restricted mean survival time into a single table.")
			}

			CheckBox
			{
				label:		qsTr("Merge plots across distributions")
				name:		"lifeTimeMergePlotsAcrossDistributions"
				checked:	false
				enabled:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value != "bestAic" && interpretModel.value != "bestBic"))
				info: qsTr("Merge the plots for survival time, survival probabilities, hazard, cumulative hazard, and restricted mean survival across distributions into a single plot. Only available when no model selection is being performed.")
			}

		}

	}

	Section
	{
		title:	qsTr("Diagnostics")

		Group
		{
			title: qsTr("Residual Plots")

			CheckBox
			{
				name:		"residualPlotResidualVsTime"
				label:		qsTr("Residuals vs. time")
				enabled:	censoringTypeRight.checked
				info: qsTr("Plot residuals versus time. Only available when Censoring Type is set to Right.")
			}

			CheckBox
			{
				name:		"residualPlotResidualVsPredictors"
				label:		qsTr("Residuals vs. predictors")
				info: qsTr("Plot residuals versus predictors to assess model fit. Available when model terms are specified.")
			}

			CheckBox
			{
				name:		"residualPlotResidualVsPredicted"
				label:		qsTr("Residuals vs. predicted survival")
				info: qsTr("Plot residuals versus predicted survival probabilities.")
			}

			CheckBox
			{
				name:		"residualPlotResidualHistogram"
				label:		qsTr("Residuals histogram")
				info: qsTr("Display a histogram of residuals to assess their distribution.")
			}

			DropDown
			{
				name:		"residualPlotResidualType"
				id:			residualPlotResidualType
				label:		qsTr("Type")
				info: qsTr("Select the type of residuals to plot")
				values:		[
					{ label: qsTr("Response"),				value: "response"},
					{ label: qsTr("Cox-Snell"),				value: "coxSnell"}
				]
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")

		Group
		{
			title:		qsTr("Selected Parametric Distributions")
			enabled:	distribution.value === "all" || distribution.value === "bestAIC" || distribution.value === "bestBIC"


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
	}
}
