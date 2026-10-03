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
	info: mixtureConstrainMinimumSpread.checked ? qsTr("This analysis models survival times as a finite mixture of components from the same parametric family. Constrained maximum likelihood imposes the specified minimum standard deviation of log survival time in every component. Several starting values, each refined by EM iterations, are used; the converged solution with the highest likelihood satisfying the bound is reported.") : qsTr("This analysis performs a parametric mixture survival analysis. The survival times are modeled as a finite mixture of components from the same parametric family. The likelihood is maximized directly from several starting values (each refined by a few EM iterations). The non-degenerate solution with the highest likelihood is reported. If all solutions are degenerate, the best of them is reported with a warning.")

	property bool	categoricalPredictionLevelsPossible:		factors.count > 0 && modelTerms.countVariables > 0
	property bool	multipleComponentsSelected:				mixtureComponents.value === "all" || mixtureComponents.value === "bestAic" || mixtureComponents.value === "bestBic"
	property bool	mergeAcrossComponentsPossible:			mixtureComponents.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))

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
			depends:	mixtureConstrainMinimumSpread
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
			].filter(function(item) {
				return !mixtureConstrainMinimumSpread.checked || ["gamma", "logLogistic", "logNormal", "weibull", "all", "bestAic", "bestBic"].indexOf(item.value) >= 0
			})
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

	SA.ParametricStatistics
	{
		multipleModels:		modelTerms.count > 1
		multipleResults:	distribution.value === "all" || (multipleComponentsSelected && mixtureMaximumComponents.value > 1) || modelTerms.count > 1
		modelSummaryInfo:	qsTr("Include a table with information about the model fit. The BIC uses the number of observations (including censored observations and weighted by the case weights) as the sample size.")

		extraStatisticsControls: [
			Group
			{
				title:	qsTr("Mixture")

				CheckBox
				{
					label:		qsTr("Mean and median")
					name:		"mixtureComponentsTable"
					checked:	false
					info: qsTr("Include a table with the mean and the median lifetime of each mixture component. They correspond to the reference level of factors and zero value of covariates. The mixing probabilities and the parameters of the components are reported in the coefficients summary. Components are ordered by their median lifetime.")
				}

				CheckBox
				{
					label:		qsTr("Classification")
					name:		"mixtureClassificationTable"
					checked:	false
					info: qsTr("Include a table with the number and proportion of observations assigned to each component based on their highest posterior probability, the mean posterior probability of the assigned observations, and the relative entropy of the classification.")
				}
			}
		]
	}

	SA.ParametricPredictions
	{
		rightCensoring:	censoringTypeRight.checked
		mergeDistributionsAvailable:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
		categoricalLevelsPossible:	categoricalPredictionLevelsPossible
		extraPlotSelected:	mixtureComponentPlot.checked
		mergeTimeComponentsActive:	survivalTimeMergePlotsAcrossComponents.checked && survivalTimeMergePlotsAcrossComponents.enabled
		mergeLifeComponentsActive:	lifeTimeMergePlotsAcrossComponents.checked && lifeTimeMergePlotsAcrossComponents.enabled
		quantileStepsInfo: qsTr("Select the quantiles at which survival times are predicted: evenly spaced Quantiles, a Sequence, or Custom probabilities.")
		quantileNumberInfo: qsTr("Specify the number of quantiles of the predicted survival when using Quantiles as the steps type.")
		lifeTimeStepsInfo: qsTr("Select the time points for prediction tables: Equal spacing, a Sequence, or Custom times. Time plots place unrounded points adaptively within the selected range.")
		lifeTimeSizeInfo: qsTr("Define the time increment when using Sequence steps. Leaving this blank uses one tenth of the selected time range.")
		lifeTimeCustomInfo: qsTr("Specify custom time points for predictions.")

		survivalTimeExtraControls: [
			CheckBox
			{
				label:		qsTr("Merge plots across components")
				id:			survivalTimeMergePlotsAcrossComponents
				name:		"survivalTimeMergePlotsAcrossComponents"
				checked:	false
				enabled:	mergeAcrossComponentsPossible
				info: qsTr("Merge the plots for survival time across the numbers of components into a single plot. Only available when all numbers of components are displayed and no model selection is being performed.")
			}
		]

		lifeTimeExtraControls: [
			CheckBox
			{
				label:		qsTr("Merge plots across components")
				id:			lifeTimeMergePlotsAcrossComponents
				name:		"lifeTimeMergePlotsAcrossComponents"
				checked:	false
				enabled:	mergeAcrossComponentsPossible
				info: qsTr("Merge the plots for survival probabilities, hazard, cumulative hazard, and restricted mean survival across the numbers of components into a single plot. Only available when all numbers of components are displayed and no model selection is being performed.")
			}
		]
	}

	Section
	{
		title:	qsTr("Diagnostics")

		Group
		{
			SA.ParametricResidualPlots
			{
				rightCensoring:	censoringTypeRight.checked
				residualVsPredictedLabel:	qsTr("Residuals vs. predicted time")
			}

			Group
			{
				title:		qsTr("Mixture")

				CheckBox
				{
					label:		qsTr("Estimation diagnostics")
					name:		"mixtureDiagnosticsTable"
					checked:	false
					info: qsTr("Include a table with the diagnostics of the estimation of each mixture model: the number of starting values, how many of them reached the reported solution, the log-likelihood of the reported and of the next best distinct solution, the number of degenerate candidate solutions, the effective sample size and the effective number of events of the smallest component, optimizer convergence, and whether the Hessian of the likelihood is positive definite.")
				}

				CheckBox
				{
					id:			mixtureComponentPlot
					label:		qsTr("Component plot")
					name:		"mixtureComponentPlot"
					info: qsTr("Include a plot with the fitted mixture and its components. Densities are averaged over the observed predictor values, separately for each factor combination unless plots are merged. Other plot types use the same covariate values as the predictions.")

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

					DropDown
					{
						name:		"mixtureComponentPlotTransformXAxis"
						label:		qsTr("X-axis transformation")
						startValue:	"log"
						info: qsTr("Select the transformation for the x-axis of the component plot. With Log selected, density curves and histogram bins are computed for log time.")
						values:
						[
							{ label: qsTr("None"),	value: "none"},
							{ label: qsTr("Log"),	value: "log"}
						]
					}

					CheckBox
					{
						name:		"mixtureComponentPlotMergePlotsAcrossFactors"
						label:		qsTr("Merge plots across factors")
						checked:	false
						enabled:	mixtureComponentPlotType.value === "density"
						info: qsTr("For density plots, average the mixture and its weighted component densities over the observed predictor values, using the proportions of observations in each factor combination and any case weights. When unchecked, show a separate plot for each observed factor combination, averaging over its observed covariate values.")
					}

					CheckBox
					{
						name:		"mixtureComponentPlotObservedData"
						label:		qsTr("Observed data")
						enabled:	censoringTypeRight.checked && mixtureComponentPlotType.value === "density"
						info: qsTr("Overlay a histogram of the observed time distribution on the fitted densities, using the same observations as the curves: each factor combination separately, or the pooled sample when plots are merged. For right-censored data, bin probabilities are estimated with Kaplan-Meier and any unobserved tail probability is retained.")
					}
				}
			}
		}

		SA.ParametricProbabilityPlot
		{
			rightCensoring:	censoringTypeRight.checked
			mergeDistributionsAvailable:	distribution.value === "all" && (modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic"))
			categoricalLevelsPossible:	categoricalPredictionLevelsPossible
			mergeComponentsActive:	probabilityPlotMergePlotsAcrossComponents.checked && probabilityPlotMergePlotsAcrossComponents.enabled

			extraMergeControls: [
				CheckBox
				{
					name:		"probabilityPlotMergePlotsAcrossComponents"
					id:			probabilityPlotMergePlotsAcrossComponents
					label:		qsTr("Merge plots across components")
					checked:	false
					enabled:	mergeAcrossComponentsPossible
					info: qsTr("Merge the probability plots across the numbers of components into a single plot. Only available when all numbers of components are displayed and no model selection is being performed.")
				}
			]
		}
	}

	SA.SurvivalExport
	{
		mixture: true
		intervalCensoring: censoringTypeInterval.checked
	}

	Section
	{
		title: qsTr("Advanced")

		Group
		{
			title:		qsTr("Selected Parametric Distributions")
			enabled:	distribution.value === "all" || distribution.value === "bestAic" || distribution.value === "bestBic"


			SA.ConstraintFamilyCheckBox
			{
				name: "selectedParametricDistributionExponential"
				label: qsTr("Exponential")
				checked: true
				depends: mixtureConstrainMinimumSpread
				constraintActive: mixtureConstrainMinimumSpread.checked
			}
			CheckBox { name: "selectedParametricDistributionGamma";						label: qsTr("Gamma");							checked: true }
			SA.ConstraintFamilyCheckBox
			{
				name: "selectedParametricDistributionGeneralizedF"
				label: qsTr("Generalized F")
				checked: true
				depends: mixtureConstrainMinimumSpread
				constraintActive: mixtureConstrainMinimumSpread.checked
			}
			SA.ConstraintFamilyCheckBox
			{
				name: "selectedParametricDistributionGeneralizedGamma"
				label: qsTr("Generalized gamma")
				checked: true
				depends: mixtureConstrainMinimumSpread
				constraintActive: mixtureConstrainMinimumSpread.checked
			}
			SA.ConstraintFamilyCheckBox
			{
				name: "selectedParametricDistributionGompertz"
				label: qsTr("Gompertz")
				checked: true
				depends: mixtureConstrainMinimumSpread
				constraintActive: mixtureConstrainMinimumSpread.checked
			}
			CheckBox { name: "selectedParametricDistributionLogLogistic";				label: qsTr("Log-logistic");					checked: true }
			CheckBox { name: "selectedParametricDistributionLogNormal";					label: qsTr("Log-normal");						checked: true }
			CheckBox { name: "selectedParametricDistributionWeibull";					label: qsTr("Weibull");							checked: true }
			SA.ConstraintFamilyCheckBox
			{
				name: "selectedParametricDistributionGeneralizedGammaOriginal"
				label: qsTr("Generalized gamma (original)")
				checked: false
				depends: mixtureConstrainMinimumSpread
				constraintActive: mixtureConstrainMinimumSpread.checked
			}
			SA.ConstraintFamilyCheckBox
			{
				name: "selectedParametricDistributionGeneralizedFOriginal"
				label: qsTr("Generalized F (original)")
				checked: false
				depends: mixtureConstrainMinimumSpread
				constraintActive: mixtureConstrainMinimumSpread.checked
			}
		}

		Group
		{
			title:		qsTr("Mixture")

			Group
			{
				CheckBox
				{
					id:			mixtureConstrainMinimumSpread
					name:		"mixtureConstrainMinimumSpread"
					label:		qsTr("Constrain minimum spread")
					checked:	false
					childrenOnSameRow:	true
					info: qsTr("Fit by constrained maximum likelihood with a minimum standard deviation of the natural logarithm of survival time in every component, including one-component models. Available for log-normal, Weibull, log-logistic, and gamma distributions; other distributions are deselected. Checkbox choices made in Selected Parametric Distributions before turning the constraint on are restored when it is turned off again, provided the analysis form has not been reloaded. When the bound is active, point estimates remain available but standard errors, confidence intervals, covariance estimates, and regular inferential tests are not reported.")

					DropDown
					{
						id:				mixtureConstrainMinimumSpreadType
						name:			"mixtureConstrainMinimumSpreadType"
						label:			""
						values: [
							{ label: qsTr("Relative"), value: "relative" },
							{ label: qsTr("Absolute"), value: "absolute" }
						]
						startValue:		"relative"
						info: qsTr("Set the minimum log-time standard deviation relative to an unconstrained one-component fit of the same distribution, or as an absolute value. The reference fit uses the same data, predictors, censoring, and weights and is computed separately for each model and subgroup.")
					}
				}

				PercentField
				{
					name:			"mixtureMinimumLogTimeSdRelative"
					label:			qsTr("Minimum log-time SD")
					defaultValue:	1
					min:			0
					max:			100
					inclusive:		JASP.MaxOnly
					decimals:		4
					visible:		mixtureConstrainMinimumSpreadType.currentValue === "relative"
					info: qsTr("Set a positive percentage of the unconstrained one-component model's log-time standard deviation as the minimum for every component. The default is 1%. For models with predictors, the reference is the conditional distribution's log-time standard deviation. For delayed entry, it refers to the distribution before conditioning on entry.")
				}

				DoubleField
				{
					name:			"mixtureMinimumLogTimeSd"
					label:			qsTr("Minimum log-time SD")
					defaultValue:	0.1
					min:			0
					max:			100
					inclusive:		JASP.MaxOnly
					decimals:		6
					visible:		mixtureConstrainMinimumSpreadType.currentValue === "absolute"
					info: qsTr("Set a positive lower bound, up to 100, on the standard deviation of the natural logarithm of survival time. Choose a minimum justified by the application and check sensitivity to other values; 0.1 is an editable preset, not a universal recommendation. The bound is unchanged when the units of survival time change. For delayed entry, it bounds the component distribution before conditioning on entry.")
				}
			}

			IntegerField
			{
				id:				mixtureMaximumComponents
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
				info: mixtureConstrainMinimumSpread.checked ? qsTr("Select the starting values of the constrained mixture estimation. The converged solution with the highest likelihood satisfying the minimum component spread is reported. More starting values reduce the risk of a local optimum at a higher computational cost.") : qsTr("Select the starting values of the mixture estimation. The likelihood is maximized directly from every selected starting value and the non-degenerate solution with the highest likelihood is reported. If all solutions are degenerate, the best of them is reported with a warning. More starting values make it more likely that the reported solution is the global maximum, at a proportionally higher computational cost.")

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
