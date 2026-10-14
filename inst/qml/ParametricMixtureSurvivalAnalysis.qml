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
		modelPredictors: models.rawPredictors
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
			selectedFamilies: distributions.selectedFamilies
			componentCount: form.multipleComponentsSelected ? mixtureMaximumComponents.value : Number(mixtureComponents.value)
		}

		DropDown
		{
			name:		"mixtureComponents"
			id:			mixtureComponents
			label:		qsTr("Components")
			startValue:	"2"
			info: qsTr("Choose the number of mixture components, or compare several numbers using All, Best AIC or Best BIC. Set the maximum in Advanced.")
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

	SA.ParametricParameterRestrictions
	{
		selector: distribution
	}

	SA.ParametricModel
	{
		analysisForm: form
		id: models
		selector: distribution
		covariateNames: variables.covariateNames
		factorNames: variables.factorNames
		predictorSourcesReady: variables.predictorSourcesReady
		mixture: true
		componentCount: distribution.componentCount
	}

	SA.ParametricStatistics
	{
		multipleModels:		models.modelCount > 1
		multipleResults:	distribution.value === "all" || (multipleComponentsSelected && mixtureMaximumComponents.value > 1) || models.modelCount > 1
		modelSummaryInfo:	qsTr("Show a table summarizing model fit.")

		extraStatisticsControls: [
			Group
			{
				title:	qsTr("Mixture")

				CheckBox
				{
					label:		qsTr("Mean and median")
					name:		"mixtureMeanAndMedian"
					checked:	false
					info: qsTr("Show the baseline mean and median lifetime of each mixture component.")
				}

				CheckBox
				{
					label:		qsTr("Classification")
					name:		"mixtureClassification"
					checked:	false
					info: qsTr("Summarize the assignment of observations to mixture components and the quality of the classification.")
				}
			}
		]
	}

	SA.ParametricPredictions
	{
		mixture: true
		mergeComponentsAvailable: mergeAcrossComponentsPossible
		analysisForm: form
		modelPredictors: models.rawPredictors
		covariateNames: variables.covariateNames
		factorNames: variables.factorNames
		predictorSourcesReady: models.predictorsInitialized && variables.predictorSourcesReady
		predictionPredictors: variables.predictionPredictors
		rightCensoring:	censoring.rightCensoring
		mergeDistributionsAvailable:	form.mergeDistributionsAvailable
		categoricalLevelsPossible:	categoricalPredictionLevelsPossible
		subgroupsAvailable:	variables.subgroupCount > 0
		extraPlotSelected:	mixtureComponentPlot.checked || densityPlot.checked
		componentPlotSelected: mixtureComponentPlot.checked
		quantileStepsInfo: qsTr("Choose how to specify lifetime percentiles: equal intervals, a sequence or custom values.")
		quantileNumberInfo: qsTr("Set the number of equal intervals between 0 and 100.")
		lifeTimeStepsInfo: qsTr("Choose how to specify prediction times: equal intervals, a sequence or custom values.")
		lifeTimeSizeInfo: qsTr("Set the time increment for the sequence. Leave blank to use the default increment.")
		lifeTimeCustomInfo: qsTr("Enter non-negative prediction times, separated by commas. Leave blank to use the default times.")


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

			SA.ParametricDensityPlot
			{
				id: densityPlot
				rightCensoring: censoring.rightCensoring
				mergeDistributionsAvailable: form.mergeDistributionsAvailable
				analysisForm: form
				modelPredictors: models.rawPredictors
				covariateNames: variables.covariateNames
				factorNames: variables.factorNames
				predictorSourcesReady: models.predictorsInitialized && variables.predictorSourcesReady
			}

			Group
			{
				title:		qsTr("Mixture")

				CheckBox
				{
					label:		qsTr("Estimation diagnostics")
					name:		"mixtureEstimationDiagnostics"
					checked:	false
					info: qsTr("Include a table with the diagnostics of the estimation of each mixture model: the number of starting values, how many of them reached the reported solution, the log-likelihood of the reported and of the next best distinct solution, the number of degenerate candidate solutions, the effective sample size and the effective number of events of the smallest component, optimizer convergence, and whether the Hessian of the likelihood is positive definite. Replications indicate agreement between starts, not proof of a global maximum.")
				}

				CheckBox
				{
					id:			mixtureComponentPlot
					label:		qsTr("Component plot")
					name:		"mixtureComponentPlot"
					info: qsTr("Show the overall mixture and the contribution of each component to the selected measure.")

					DropDown
					{
						name:		"mixtureComponentPlotType"
						id:			mixtureComponentPlotType
						label:		qsTr("Type")
						startValue:	"density"
						fieldWidth: 150 * jaspTheme.uiScale
						info: qsTr("Choose the measure to display in the component plot.")
						values:
						[
							{ label: qsTr("Survival probability"),	value: "survival"},
							{ label: qsTr("Failure probability"),	value: "failureProbability"},
							{ label: qsTr("Density"),				value: "density"},
							{ label: qsTr("Hazard"),				value: "hazard"},
							{ label: qsTr("Cumulative hazard"), 	value: "cumulativeHazard"},
							{ label: qsTr("Restricted mean survival time"), value: "restrictedMeanSurvivalTime"}
						]
					}

					DropDown
					{
						name:		"mixtureComponentPlotXAxisTransformation"
						label:		qsTr("X-axis transformation")
						startValue:	"log"
						info: qsTr("Choose the transformation for the x-axis of the component plot.")
						values:
						[
							{ label: qsTr("None"),	value: "none"},
							{ label: qsTr("Log"),	value: "log"}
						]
					}

					DropDown
					{
						name: "mixtureComponentPlotYAxisTransformation"
						label: qsTr("Y-axis transformation")
						startValue: "none"
						values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}]
						info: qsTr("Display the selected measure on a linear or logarithmic y-axis.")
					}

					SA.ParametricObservedStrata
					{
						prefix: "mixtureComponentPlot"
						analysisForm: form
						modelPredictors: models.rawPredictors
						covariateNames: variables.covariateNames
						factorNames: variables.factorNames
						predictorSourcesReady: models.predictorsInitialized && variables.predictorSourcesReady
					}

					Group
					{
						title: qsTr("Merge Plots Across")
						columns: 2

						CheckBox
						{
							name: "mixtureComponentPlotMergePlotsAcrossDistributions"
							label: qsTr("Distributions")
							checked: false
							enabled: form.mergeDistributionsAvailable
							info: qsTr("Overlay the component plots for the selected distributions.")
						}

					}

					DropDown
					{
						name: "mixtureComponentPlotLegend"
						label: qsTr("Legend")
						startValue: "right"
						info: qsTr("Choose the legend position for the component plot, or hide the legend.")
						values:
						[
							{ label: qsTr("Bottom"), value: "bottom"},
							{ label: qsTr("Right"), value: "right"},
							{ label: qsTr("Left"), value: "left"},
							{ label: qsTr("Top"), value: "top"},
							{ label: qsTr("None"), value: "none"}
						]
					}

				}
			}
		}

		SA.ParametricProbabilityPlot
		{
			analysisForm: form
			modelPredictors: models.rawPredictors
			covariateNames: variables.covariateNames
			factorNames: variables.factorNames
			predictorSourcesReady: models.predictorsInitialized && variables.predictorSourcesReady
			rightCensoring:	censoring.rightCensoring
			mergeDistributionsAvailable:	form.mergeDistributionsAvailable
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
					info: qsTr("Overlay probability plots for different numbers of mixture components.")
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
			id: distributions
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
					info: qsTr("Set a minimum spread for each mixture component to avoid very narrow components. Uncertainty estimates are unavailable with this constraint.")

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
						info: qsTr("Express the minimum spread relative to a one-component model, or as an absolute value.")
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
					info: qsTr("Set the minimum log-time standard deviation as a percentage of that in a one-component model.")
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
					info: qsTr("Set the minimum standard deviation of log survival time for each component.")
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
				info: qsTr("Set the maximum number of components to compare when All, Best AIC or Best BIC is selected.")
			}

			Group
			{
				title:		qsTr("Starting Values")
				info: mixtureConstrainMinimumSpread.checked ? qsTr("Select starting values for constrained mixture estimation. The best converged solution found within the minimum spread bound is reported. More starts reduce the risk of a local optimum but take longer.") : qsTr("Select starting values for mixture estimation. The best non-degenerate solution found is reported. If all solutions are degenerate, the best is reported with a warning. More starts reduce the risk of a local optimum but take longer.")

				CheckBox
				{
					name:		"mixtureStartingValuesKMeans"
					label:		qsTr("K-means")
					checked:	true
					info: qsTr("Use k-means clustering of log survival times to obtain starting values.")
				}

				CheckBox
				{
					name:		"mixtureStartingValuesQuantiles"
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
					name:		"mixtureStartingValuesSplitOfThePreviousSolution"
					label:		qsTr("Split simpler model")
					checked:	true
					info: qsTr("Start from a model with one component fewer. Each start splits one of its components at its median, keeping the other components unchanged. The simpler model is fitted when needed.")
				}

				CheckBox
				{
					name:				"mixtureStartingValuesRandom"
					label:				qsTr("Random")
					checked:			true
					childrenOnSameRow:	true
					info: qsTr("Draw independent sets of random centres from the observed event times and assign each observation to the nearest centre. Higher values try more sets of centres.")

					IntegerField
					{
						name:			"mixtureStartingValuesRandomCount"
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
					name: "outputFormattingCompareModelsAcrossComponents"
					text: qsTr("Compare models across components")
					enabled: multipleComponentsSelected && models.modelCount > 1
					checked: true
					info: qsTr("Compare models across different numbers of mixture components.")
				}
			]
		}

		SA.ParametricSimulation {}
	}
}
