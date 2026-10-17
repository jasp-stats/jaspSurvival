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
	id: form
	info: qsTr("This analysis models survival times as a finite mixture of components from the same parametric family.")

	property bool	categoricalPredictionLevelsPossible:		variables.factorCount > 0 && models.variableCount > 0
	property bool	mergeDistributionsAvailable:				distribution.value === "all" && models.selectionAllowsMerging
	property bool	multipleComponentsSelected:				mixtureComponents.value === "all" || mixtureComponents.value === "bestAic" || mixtureComponents.value === "bestBic"
	property bool	mergeAcrossComponentsPossible:			mixtureComponents.value === "all" && models.selectionAllowsMerging

	SA.ParametricVariables
	{
		id: variables
		mixture: true
		rightCensoring: censoring.rightCensoring
		intervalCensoring: censoring.intervalCensoring
		countingCensoring: censoring.countingCensoring
	}

	SA.ParametricCensoring
	{
		id: censoring
		mixture: true
	}

	Group
	{
		SA.ParametricDistribution
		{
			id: distribution
			mixture: true
			depends: mixtureConstrainMinimumSpread
			constraintActive: mixtureConstrainMinimumSpread.checked
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

	SA.ParametricModel
	{
		id: models
		mixture: true
	}

	SA.ParametricStatistics
	{
		multipleModels:		models.modelCount > 1
		multipleResults:	distribution.value === "all" || (multipleComponentsSelected && mixtureMaximumComponents.value > 1) || models.modelCount > 1
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
		rightCensoring:	censoring.rightCensoring
		mergeDistributionsAvailable:	form.mergeDistributionsAvailable
		categoricalLevelsPossible:	categoricalPredictionLevelsPossible
		subgroupsAvailable:	variables.subgroupCount > 0
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
				label:		qsTr("Components")
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
				label:		qsTr("Components")
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
				rightCensoring:	censoring.rightCensoring
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
					info: qsTr("Include a table with the diagnostics of the estimation of each mixture model: the number of starting values, how many of them reached the reported solution, the log-likelihood of the reported and of the next best distinct solution, the number of degenerate candidate solutions, the effective sample size and the effective number of events of the smallest component, optimizer convergence, and whether the Hessian of the likelihood is positive definite. Replications indicate agreement between starts, not proof of a global maximum.")
				}

				CheckBox
				{
					id:			mixtureComponentPlot
					label:		qsTr("Component plot")
					name:		"mixtureComponentPlot"
					info: qsTr("Include a plot with the fitted mixture and its components. Densities are averaged over the observed predictor values, separately for each factor combination unless plots are merged. Other plot types use the combined prediction convention: observed levels for models with categorical predictors only, or the pooled unweighted mean model design for models with numeric predictors.")

					DropDown
					{
						name:		"mixtureComponentPlotType"
						id:			mixtureComponentPlotType
						label:		qsTr("Type")
						startValue:	"density"
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
						checked:	true
						enabled:	censoring.rightCensoring && mixtureComponentPlotType.value === "density"
						info: qsTr("Overlay a histogram of the observed time distribution on the fitted densities, using the same observations as the curves: each factor combination separately, or the pooled sample when plots are merged. For right-censored data, bin probabilities are estimated with Kaplan-Meier and any unobserved tail probability is retained.")

						DropDown
						{
							name:		"mixtureComponentPlotHistogramBinWidthType"
							id:			mixtureComponentPlotHistogramBinWidthType
							label:		qsTr("Bin width type")
							startValue:	"fd"
							info: qsTr("Select the rule for histogram bin widths. With Log selected, the rule is applied to log time. Select Manual to specify the number of bins.")
							values:
							[
								{ label: qsTr("Sturges"),			value: "sturges" },
								{ label: qsTr("Scott"),				value: "scott" },
								{ label: qsTr("Doane"),				value: "doane" },
								{ label: qsTr("Freedman-Diaconis"),	value: "fd" },
								{ label: qsTr("Manual"),				value: "manual" }
							]
						}

						IntegerField
						{
							name:			"mixtureComponentPlotHistogramManualNumberOfBins"
							label:			qsTr("Number of bins")
							defaultValue:	30
							min:			3
							max:			10000
							enabled:		mixtureComponentPlotHistogramBinWidthType.currentValue === "manual"
							info: qsTr("Specify the target number of histogram bins. Bin boundaries are rounded to convenient values, as in Descriptive Statistics.")
						}
					}
				}
			}
		}

		SA.ParametricProbabilityPlot
		{
			rightCensoring:	censoring.rightCensoring
			mergeDistributionsAvailable:	form.mergeDistributionsAvailable
			categoricalLevelsPossible:	categoricalPredictionLevelsPossible
			subgroupsAvailable:	variables.subgroupCount > 0
			mergeComponentsActive:	probabilityPlotMergePlotsAcrossComponents.checked && probabilityPlotMergePlotsAcrossComponents.enabled

			extraMergeControls: [
				CheckBox
				{
					name:		"probabilityPlotMergePlotsAcrossComponents"
					id:			probabilityPlotMergePlotsAcrossComponents
					label:		qsTr("Components")
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
		intervalCensoring: censoring.intervalCensoring
	}

	Section
	{
		title: qsTr("Advanced")

		SA.ParametricDistributions
		{
			selectedDistribution: distribution.value
			constraintControl: mixtureConstrainMinimumSpread
			constraintActive: mixtureConstrainMinimumSpread.checked
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
				info: mixtureConstrainMinimumSpread.checked ? qsTr("Select starting values for constrained mixture estimation. The best converged solution found within the minimum spread bound is reported. More starts reduce the risk of a local optimum but take longer.") : qsTr("Select starting values for mixture estimation. The best non-degenerate solution found is reported. If all solutions are degenerate, the best is reported with a warning. More starts reduce the risk of a local optimum but take longer.")

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
					label:		qsTr("Equal-sized groups")
					checked:	true
					info: qsTr("Start from one partition of the ordered survival times into groups of equal size.")
				}

				CheckBox
				{
					name:		"mixtureStartTails"
					label:		qsTr("Tail partitions")
					checked:	true
					info: qsTr("Start from partitions that isolate the tails: 15/85% and 85/15% for two components, 15/70/15% for three, and 10/40/40/10% for four.")
				}

				CheckBox
				{
					name:		"mixtureStartSplit"
					label:		qsTr("Split simpler model")
					checked:	true
					info: qsTr("Start from a model with one component fewer. Each start splits one of its components at its median, keeping the other components unchanged. The simpler model is fitted when needed.")
				}

				CheckBox
				{
					name:				"mixtureStartRandom"
					label:				qsTr("Random")
					checked:			true
					childrenOnSameRow:	true
					info: qsTr("Draw independent sets of random centres from the observed event times and assign each observation to the nearest centre. Higher values try more sets of centres.")

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

			Group
			{
				title:		qsTr("Initialization Settings")
				info: qsTr("Apply these settings to every starting value method.")

				FormulaField
				{
					name:			"mixtureStartMembershipProbabilities"
					label:			qsTr("Membership probability")
					defaultValue:	"0.95, 0.99"
					// Desktop's formula validation expects a scalar; the analysis validates this vector.
					inputType:		"string"
					fieldWidth:		160 * jaspTheme.uiScale
					info: qsTr("Higher probabilities produce sharper starts. Each starting value is tried with every initial membership probability.")
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
		}

		SA.ParametricOutputFormatting
		{
			mixture: true
			hasSubgroup: variables.subgroupCount > 0
			multipleModels: models.modelCount > 1
			selectedDistribution: distribution.value
			extraControls: [
				CheckBox
				{
					name: "compareModelsAcrossComponents"
					text: qsTr("Compare models across components")
					enabled: multipleComponentsSelected && models.modelCount > 1
					checked: true
					info: qsTr("Compare models across the numbers of components. This option is only available when multiple models and numbers of components are specified.")
				}
			]
		}

		SA.ParametricSimulation {}
	}
}
