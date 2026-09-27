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
	info: qsTr("This analysis performs a parametric mixture survival analysis. The survival times are modeled as a finite mixture of components from the same parametric family. The likelihood is maximized directly from several starting values (each refined by a few EM iterations). The non-degenerate solution with the highest likelihood is reported. If all solutions are degenerate, the best of them is reported with a warning.")

	property bool	categoricalPredictionLevelsPossible:		factors.count > 0 && modelTerms.countVariables > 0
	property bool	multipleComponentsSelected:				mixtureComponents.value === "all" || mixtureComponents.value === "bestAic" || mixtureComponents.value === "bestBic"
	property bool	mergeAcrossComponentsPossible:			mixtureComponents.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
	property bool	survivalTimeSeriesControlsAvailable:		(survivalTimeMergePlotsAcrossDistributions.checked && survivalTimeMergePlotsAcrossDistributions.enabled) || (survivalTimeMergePlotsAcrossComponents.checked && survivalTimeMergePlotsAcrossComponents.enabled) || categoricalPredictionLevelsPossible
	property bool	lifeTimeSeriesControlsAvailable:			(lifeTimeMergePlotsAcrossDistributions.checked && lifeTimeMergePlotsAcrossDistributions.enabled) || (lifeTimeMergePlotsAcrossComponents.checked && lifeTimeMergePlotsAcrossComponents.enabled) || categoricalPredictionLevelsPossible
	property bool	lifeTimePlotSelected:					survivalProbabilityPlot.checked || hazardPlot.checked || cumulativeHazardPlot.checked || restrictedMeanSurvivalTimePlot.checked
	property bool	predictionPlotSelected:					survivalTimePlot.checked || lifeTimePlotSelected || mixtureComponentPlot.checked
	property bool	predictionLegendPaletteAvailable:			(survivalTimePlot.checked && survivalTimeSeriesControlsAvailable) || (lifeTimePlotSelected && lifeTimeSeriesControlsAvailable) || mixtureComponentPlot.checked
	property bool	probabilityPlotLegendPaletteAvailable:	probabilityPlotFittedCurve.checked && ((probabilityPlotMergePlotsAcrossDistributions.checked && probabilityPlotMergePlotsAcrossDistributions.enabled) || (probabilityPlotMergePlotsAcrossComponents.checked && probabilityPlotMergePlotsAcrossComponents.enabled) || categoricalPredictionLevelsPossible)

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
			info: qsTr("Add continuous variables as covariates to include them in the mixture model. The covariates affect the location parameter of each component with separate coefficients.")
		}

		AssignedVariablesList
		{
			id:				 	factors
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["nominal"]
			info: qsTr("Add categorical variables as factors to include them in the mixture model. The factors affect the location parameter of each component with separate coefficients.")
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
			info: qsTr("Select right-censored data, counting-process data with entry and exit times, or interval-censored data (including left-censored observations).")

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

	Group
	{
		DropDown
		{
			name:		"distribution"
			id:			distribution
			label:		qsTr("Distribution")
			startValue:	"weibull"
			info: qsTr("Choose the parametric distribution of the mixture components (all components come from the same distribution). All fits and display results for all 'Selected parametric families' in the 'Advanced' section. 'Best AIC' and 'Best BIC' fit all `Selected parametric families` in the Advanced section and display the results only for a parametric family with the lowest AIC/BIC. Families without a closed-form weighted fit (gamma, Gompertz, and the generalized families) are considerably slower to estimate.")
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

		DropDown
		{
			name:		"mixtureComponents"
			id:			mixtureComponents
			label:		qsTr("Components")
			startValue:	"2"
			info: qsTr("Choose the number of mixture components. 'All' fits and displays results for one up to the maximum number of components set in the 'Advanced' section. 'Best AIC' and 'Best BIC' fit the same models and display the results only for the number of components with the lowest AIC/BIC. A fixed number of components is not limited by the maximum number of components.")
			values:
			[
				{ label: "1",					value: "1"},
				{ label: "2",					value: "2"},
				{ label: "3",					value: "3"},
				{ label: "4",					value: "4"},
				{ label: qsTr("All"),			value: "all"},
				{ label: qsTr("Best AIC"),		value: "bestAic"},
				{ label: qsTr("Best BIC"),		value: "bestBic"}
			]
		}
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
			info: qsTr("Select the model to interpret. Defaults to the last specified model. Alternatives are 'All' which produces results for all of the specified models or 'Best' which produces results for the best fitting model based on either AIC or BIC. The selection proceeds within each subgroup from the distribution to the number of components to the model: 'Best' keeps the level of the best fitting model across all remaining distributions, numbers of components, and models, and 'All' selects separately within each of its levels (e.g., all distributions with the best number of components and the best model within each distribution).")
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
			info: qsTr("Include a table with information about the model fit. The BIC uses the number of observations (including censored observations and weighted by the case weights) as the sample size.")

			CheckBox
			{
				name:		"modelSummaryRankModels"
				label:		qsTr("Rank models")
				enabled:	distribution.value === "all" || mixtureComponents.value === "all" || modelTerms.count > 1
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
				enabled:	distribution.value === "all" || mixtureComponents.value === "all" || modelTerms.count > 1
				info: qsTr("Include AIC weights in the model summary.")
			}

			CheckBox
			{
				name:		"modelSummaryBicWeighs"
				label:		qsTr("BIC weights")
				enabled:	distribution.value === "all" || mixtureComponents.value === "all" || modelTerms.count > 1
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

				CheckBox
				{
					name:				"coefficientsConfidenceInterval"
					label:				qsTr("Confidence intervals")
					checked:			true
					childrenOnSameRow:	true
					info: qsTr("Include confidence intervals for the coefficients.")

					CIField
					{
						name: "coefficientsConfidenceIntervalLevel"
						info: qsTr("Set the confidence level for the confidence intervals.")
					}
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

		Group
		{
			title:	qsTr("Mixture")

			CheckBox
			{
				label:		qsTr("Mean and median")
				name:		"mixtureComponentsTable"
				checked:	true
				info: qsTr("Include a table with the mean and the median lifetime of each mixture component. They correspond to the reference level of factors and zero value of covariates. The mixing probabilities and the parameters of the components are reported in the coefficients summary. Components are ordered by their median lifetime.")
			}

			CheckBox
			{
				label:		qsTr("Classification")
				name:		"mixtureClassificationTable"
				checked:	false
				info: qsTr("Include a table with the number and proportion of observations assigned to each component based on their highest posterior probability, the mean posterior probability of the assigned observations, and the relative entropy of the classification.")
			}

			CheckBox
			{
				label:		qsTr("Estimation diagnostics")
				name:		"mixtureDiagnosticsTable"
				checked:	false
				info: qsTr("Include a table with the diagnostics of the estimation of each mixture model: the number of starting values, how many of them reached the reported solution, the log-likelihood of the reported and of the next best distinct solution, the number of degenerate candidate solutions, the effective sample size and the effective number of events of the smallest component, optimizer convergence, and whether the Hessian of the likelihood is positive definite.")
			}
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
					id:			survivalTimePlot
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
				info: qsTr("Select the quantiles at which survival times are predicted: evenly spaced Quantiles, a Sequence, or Custom probabilities.")
				values:
				[
					{ label: qsTr("Quantiles"),	value: "quantiles"},
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
				info: qsTr("Specify the number of quantiles of the predicted survival when using Quantiles as the steps type.")
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
				id:			survivalTimeMergePlotsAcrossDistributions
				name:		"survivalTimeMergePlotsAcrossDistributions"
				checked:	false
				enabled:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
				info: qsTr("Merge the plots for survival probability across distributions into a single plot. Only available when no model selection is being performed.")
			}

			CheckBox
			{
				label:		qsTr("Merge plots across components")
				id:			survivalTimeMergePlotsAcrossComponents
				name:		"survivalTimeMergePlotsAcrossComponents"
				checked:	false
				enabled:	mergeAcrossComponentsPossible
				info: qsTr("Merge the plots for survival time across the numbers of components into a single plot. Only available when all numbers of components are displayed and no model selection is being performed.")
			}


			Group
			{
				title:		qsTr("Options")

				CheckBox
				{
					label:				qsTr("Confidence intervals")
					name:				"predictionsConfidenceInterval"
					checked:			true
					childrenOnSameRow:	true
					info: qsTr("Include confidence intervals for the figures and tables.")

					CIField
					{
						name: "predictionsConfidenceIntervalLevel"
						info: qsTr("Set the confidence level for the confidence intervals.")
					}
				}

				DropDown
				{
					name:		"plotLegend"
					label:		qsTr("Legend")
					startValue:	"right"
					enabled:	predictionLegendPaletteAvailable
					info: qsTr("Choose the position of the legend on prediction plots with multiple displayed curves.")
					values:
					[
						{ label: qsTr("Bottom"),	value: "bottom"},
						{ label: qsTr("Right"),		value: "right"},
						{ label: qsTr("Left"),		value: "left"},
						{ label: qsTr("Top"),		value: "top"},
						{ label: qsTr("None"),		value: "none"}
					]
				}

				ColorPalette
				{
					enabled:	predictionLegendPaletteAvailable
					info: qsTr("Customize the color palette used in prediction plots with multiple displayed curves.")
				}

				DropDown
				{
					name:		"plotTheme"
					label:		qsTr("Theme")
					startValue:	"jasp"
					enabled:	predictionPlotSelected
					info: qsTr("Select the theme for prediction plots. The detailed theme works only for 'Survival probabilities' plots.")
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
					id:			survivalProbabilityPlot
					label:		qsTr("Plot")
					name:		"survivalProbabilityPlot"
					info: qsTr("Include a plot with the predicted survival probabilities.")

					CheckBox
					{
						name:		"survivalProbabilityPlotKaplanMeier"
						label:		qsTr("Kaplan-Meier")
						enabled:	censoringTypeRight.checked
						info: qsTr("Show a Kaplan-Meier curve in the plot. Only available when Censoring Type is set to Right.")
					}

					CheckBox
					{
						name:		"survivalProbabilityPlotCensoringEvents"
						label:		qsTr("Censoring events")
						enabled:	censoringTypeRight.checked
						info: qsTr("Show censoring events as rug marks in the plot. Only available when Censoring Type is set to Right.")
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
					id:		hazardPlot
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
					id:		cumulativeHazardPlot
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
					id:		restrictedMeanSurvivalTimePlot
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
				info: qsTr("Select the time points at which predictions are evaluated: Equal spacing, a Sequence, or Custom times.")
				values:
				[
					{ label: qsTr("Equal spacing"),	value: "quantiles"},
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
				info: qsTr("Specify the number of time points when using Equal spacing as the steps type.")
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
				info: qsTr("Set the starting time when using Sequence steps.")
			}

			FormulaField
			{
				name:			"predictionsLifeTimeStepsSize"
				id:				predictionsLifeTimeStepsSize
				label:			qsTr("Size")
				defaultValue:	""
				visible:		predictionsLifeTimeStepsType.value === "sequence"
				fieldWidth:		40 * jaspTheme.uiScale
				info: qsTr("Define the time increment when using Sequence steps. Leaving this blank uses one tenth of the selected time range.")
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
				info: qsTr("Set the ending time when using Sequence steps. Leaving this blank uses the maximum observed time.")
			}

			CheckBox
			{
				name:		"predictionsLifeTimeRoundSteps"
				label:		qsTr("Round steps")
				checked:	true
				visible:	predictionsLifeTimeStepsType.value === "quantiles" || predictionsLifeTimeStepsType.value === "sequence"
				info: qsTr("Round the time points to the nearest integer when using Equal spacing or Sequence steps.")
			}

			FormulaField
			{
				name:			"predictionsLifeTimeCustom"
				label:			qsTr("Steps")
				visible:		predictionsLifeTimeStepsType.value === "custom"
				defaultValue:	"0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9"
				info: qsTr("Specify custom time points for predictions.")
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
				id:			lifeTimeMergePlotsAcrossDistributions
				name:		"lifeTimeMergePlotsAcrossDistributions"
				checked:	false
				enabled:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
				info: qsTr("Merge the plots for survival time, survival probabilities, hazard, cumulative hazard, and restricted mean survival across distributions into a single plot. Only available when no model selection is being performed.")
			}

			CheckBox
			{
				label:		qsTr("Merge plots across components")
				id:			lifeTimeMergePlotsAcrossComponents
				name:		"lifeTimeMergePlotsAcrossComponents"
				checked:	false
				enabled:	mergeAcrossComponentsPossible
				info: qsTr("Merge the plots for survival probabilities, hazard, cumulative hazard, and restricted mean survival across the numbers of components into a single plot. Only available when all numbers of components are displayed and no model selection is being performed.")
			}

		}

		Group
		{
			title:		qsTr("Mixture Components")

			CheckBox
			{
				id:			mixtureComponentPlot
				label:		qsTr("Plot")
				name:		"mixtureComponentPlot"
				info: qsTr("Include a plot with the fitted mixture and its components. The components are evaluated at the same covariate values as the predictions.")

				DropDown
				{
					name:		"mixtureComponentPlotType"
					id:			mixtureComponentPlotType
					label:		qsTr("Type")
					startValue:	"survival"
					info: qsTr("Select the displayed function: the survival probability of the mixture and of each component, the failure probability of the mixture and of each component, the density of the mixture and the weighted density of each component (which sum to the mixture density), or the hazard of the mixture and of each component.")
					values:
					[
						{ label: qsTr("Survival probability"),	value: "survival"},
						{ label: qsTr("Failure probability"),	value: "failureProbability"},
						{ label: qsTr("Density"),				value: "density"},
						{ label: qsTr("Hazard"),				value: "hazard"}
					]
				}

				CheckBox
				{
					name:		"mixtureComponentPlotKaplanMeier"
					label:		qsTr("Kaplan-Meier")
					enabled:	censoringTypeRight.checked && (mixtureComponentPlotType.value === "survival" || mixtureComponentPlotType.value === "failureProbability")
					info: qsTr("Show a Kaplan-Meier curve in the survival or failure probability plot. Only available when Censoring Type is set to Right.")
				}
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
				label:		qsTr("Residuals vs. predicted time")
				info: qsTr("Plot residuals versus predicted mean survival times.")
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

		CheckBox
		{
			id:			probabilityPlot
			name:		"probabilityPlot"
			label:		qsTr("Probability plot")
			enabled:	censoringTypeRight.checked
			info: qsTr("Create a model-based probability plot to assess how well the selected parametric distribution describes the observed failure times. Only available when Censoring Type is set to Right.")

			DropDown
			{
				name:		"probabilityPlotCanvas"
				label:		qsTr("Canvas")
				startValue:	"weibull"
				info: qsTr("Select the probability-paper scale. A distribution matching the selected canvas appears approximately as a straight line. The exponential canvas uses linear time and cumulative-hazard probability scaling; the other canvases use log time.")
				values:
				[
					{ label: qsTr("Weibull"),		value: "weibull"},
					{ label: qsTr("Exponential"),	value: "exponential"},
					{ label: qsTr("Log-normal"),	value: "lognormal"},
					{ label: qsTr("Log-logistic"),	value: "loglogistic"}
				]
			}

			CheckBox
			{
				name:		"probabilityPlotEmpiricalPoints"
				label:		qsTr("Empirical points")
				checked:	true
				info: qsTr("Plot empirical failure probability points based on the observed failure times.")

				CheckBox
				{
					name:		"probabilityPlotPointCoordinates"
					label:		qsTr("Coordinates")
					checked:	false
					info: qsTr("Display the time and failure probability next to each empirical point.")
				}
			}

			CheckBox
			{
				name:		"probabilityPlotFittedCurve"
				id:			probabilityPlotFittedCurve
				label:		qsTr("Fitted curve")
				checked:	true
				info: qsTr("Plot the fitted curve from the selected parametric model.")
			}

			CheckBox
			{
				name:		"probabilityPlotCensoringEvents"
				label:		qsTr("Censoring events")
				enabled:	censoringTypeRight.checked
				info: qsTr("Show censored observations as rug marks at the bottom of the plot. Only available when Censoring Type is set to Right.")
			}

			CheckBox
			{
				name:		"probabilityPlotMergePlotsAcrossDistributions"
				id:			probabilityPlotMergePlotsAcrossDistributions
				label:		qsTr("Merge plots across distributions")
				checked:	false
				enabled:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
				info: qsTr("Merge the probability plots across distributions into a single plot. Only available when no model selection is being performed.")
			}

			CheckBox
			{
				name:		"probabilityPlotMergePlotsAcrossComponents"
				id:			probabilityPlotMergePlotsAcrossComponents
				label:		qsTr("Merge plots across components")
				checked:	false
				enabled:	mergeAcrossComponentsPossible
				info: qsTr("Merge the probability plots across the numbers of components into a single plot. Only available when all numbers of components are displayed and no model selection is being performed.")
			}

			CheckBox
			{
				name:				"probabilityPlotConfidenceInterval"
				label:				qsTr("Confidence intervals")
				checked:			true
				childrenOnSameRow:	true
				info: qsTr("Include confidence interval bounds for the fitted probability curve.")

				CIField
				{
					name:			"probabilityPlotConfidenceIntervalLevel"
					defaultValue:	90
					info: qsTr("Set the confidence level for the probability plot confidence intervals.")
				}
			}

			CheckBox
			{
				name:		"probabilityPlotGrid"
				label:		qsTr("Grid")
				checked:	true
				info: qsTr("Display probability-paper grid lines.")
			}

			DropDown
			{
				name:		"probabilityPlotPlottingPosition"
				label:		qsTr("Plotting position")
				startValue:	"median"
				info: qsTr("Select the method used to compute empirical failure probability plotting positions.")
				values:
				[
					{ label: qsTr("Median"),			value: "median"},
					{ label: qsTr("Benard"),			value: "benard"},
					{ label: qsTr("Hazen"),				value: "hazen"},
					{ label: qsTr("Mean"),				value: "mean"},
					{ label: qsTr("Kaplan-Meier"),		value: "kaplanMeier"},
					{ label: qsTr("Blom"),				value: "blom"}
				]
			}

			DropDown
			{
				name:		"probabilityPlotRankAdjustment"
				label:		qsTr("Rank adjustment")
				startValue:	"johnson"
				info: qsTr("Select the rank adjustment method for right-censored observations. The Kaplan-Meier adjustment follows the WeibullR/Minitab convention, including the final-failure adjustment used to keep points finite on probability paper.")
				values:
				[
					{ label: qsTr("Johnson"),		value: "johnson"},
					{ label: qsTr("Kaplan-Meier"),	value: "kaplanMeier"}
				]
			}

			DropDown
			{
				name:		"probabilityPlotTiesHandler"
				label:		qsTr("Ties")
				startValue:	"none"
				info: qsTr("Select how tied failure times are handled when computing empirical plotting positions.")
				values:
				[
					{ label: qsTr("None"),			value: "none"},
					{ label: qsTr("Highest"),		value: "highest"},
					{ label: qsTr("Lowest"),			value: "lowest"},
					{ label: qsTr("Mean"),			value: "mean"},
					{ label: qsTr("Sequential"),		value: "sequential"}
				]
			}

			DropDown
			{
				name:		"probabilityPlotLegend"
				label:		qsTr("Legend")
				startValue:	"right"
				enabled:	probabilityPlotLegendPaletteAvailable
				info: qsTr("Choose the legend position for probability plots with multiple fitted curves.")
				values:
				[
					{ label: qsTr("Bottom"),	value: "bottom"},
					{ label: qsTr("Right"),		value: "right"},
					{ label: qsTr("Left"),		value: "left"},
					{ label: qsTr("Top"),		value: "top"},
					{ label: qsTr("None"),		value: "none"}
				]
			}

			ColorPalette
			{
				name:		"probabilityPlotColorPalette"
				enabled:	probabilityPlotLegendPaletteAvailable
				info: qsTr("Customize the color palette used in probability plots with multiple fitted curves.")
			}

			DropDown
			{
				name:		"probabilityPlotTheme"
				label:		qsTr("Theme")
				startValue:	"jasp"
				enabled:	probabilityPlot.checked
				info: qsTr("Select the theme for the probability plot's appearance.")
				values:
				[
					{ label: "JASP",					value: "jasp"},
					{ label: qsTr("Detailed"),			value: "detailed"},
					{ label: qsTr("White background"),	value: "whiteBackground"},
					{ label: qsTr("Light"),				value: "light"},
					{ label: qsTr("Minimal")	,		value: "minimal"},
					{ label: "APA",						value: "apa"},
					{ label: "pubr",					value: "pubr"}
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
			title:		qsTr("Mixture")

			IntegerField
			{
				name:			"mixtureMaximumComponents"
				label:			qsTr("Maximum components")
				defaultValue:	4
				min:			1
				max:			4
				enabled:		multipleComponentsSelected
				info: qsTr("Set the maximum number of components fitted when 'All', 'Best AIC', or 'Best BIC' is selected as the number of components.")
			}

			Group
			{
				title:		qsTr("Starting Values")
				info: qsTr("Select the starting values of the mixture estimation. The likelihood is maximized directly from every selected starting value and the non-degenerate solution with the highest likelihood is reported. If all solutions are degenerate, the best of them is reported with a warning. More starting values make it more likely that the reported solution is the global maximum, at a proportionally higher computational cost.")

				CheckBox
				{
					name:		"mixtureStartKmeans"
					label:		qsTr("K-means")
					checked:	true
					info: qsTr("Start from a k-means clustering of the log survival times. The likelihood is maximized directly from this starting value.")
				}

				CheckBox
				{
					name:		"mixtureStartQuantiles"
					label:		qsTr("Quantiles")
					checked:	true
					info: qsTr("Start from partitions of the survival times by their quantiles: a partition into groups of equal size and partitions that isolate the tails (the lowest and the highest 15% for two components, 15/70/15% for three components, and 10/40/40/10% for four components). The likelihood is maximized directly from each of these starting values.")
				}

				CheckBox
				{
					name:		"mixtureStartSplit"
					label:		qsTr("Split of the previous solution")
					checked:	true
					info: qsTr("Start from the solution with one component fewer, splitting each of its components into a lower and an upper half. The likelihood is maximized directly from each of these starting values. The solution with one component fewer is estimated for this purpose when it is not part of the analysis.")
				}

				CheckBox
				{
					name:				"mixtureStartRandom"
					label:				qsTr("Random")
					checked:			true
					childrenOnSameRow:	true
					info: qsTr("Start from random centres drawn from the observed event times, assigning each observation to the nearest centre. The likelihood is maximized directly from each of these starting values. Random starting values are the main protection against reporting a local optimum.")

					IntegerField
					{
						name:			"mixtureStartRandomCount"
						label:			""
						defaultValue:	10
						min:			1
						max:			50
						info: qsTr("Set the number of random starting values.")
					}
				}
			}

			IntegerField
			{
				name:			"mixtureEmIterations"
				label:			qsTr("EM iterations")
				defaultValue:	10
				min:			1
				max:			1000
				info: qsTr("Set the number of EM iterations that refine each starting value before the likelihood is maximized directly. The state after the first and after the last EM iteration are both used as starting values of the direct maximization.")
			}

			SetSeed {}
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
				name:		"compareModelsAcrossComponents"
				text:		qsTr("Compare models across components")
				enabled:	multipleComponentsSelected && modelTerms.count > 1
				checked:	true
				info: qsTr("Compare models across the numbers of components. This option is only available when multiple models and numbers of components are specified.")
			}

			CheckBox
			{
				name:		"alwaysDisplayModelInformation"
				text:		qsTr("Always display model information")
				checked:	false
				info: qsTr("Always display model information (distribution, number of components, and model name) in output tables.")
			}
		}
	}
}
