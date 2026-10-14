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
// Preserve translation keys used by the published parametric form.
pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id: predictionsRoot
	property var analysisForm
	property bool mixture: false
	property bool rightCensoring: false
	property bool mergeDistributionsAvailable: false
	property bool mergeComponentsAvailable: false
	property bool categoricalLevelsPossible: false
	property bool subgroupsAvailable: false
	property bool extraPlotSelected: false
	property bool componentPlotSelected: false
	property var modelPredictors: []
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false
	property var predictionPredictors: []
	readonly property bool variableInput: predictionInput.value === "variables"
	readonly property var axisVariables: predictionsRoot.predictorView ? predictionRoles.axisVariables : contourRoles.axes
	readonly property var lineVariables: predictorView ? predictionRoles.lineVariables : contourRoles.lineVariables
	readonly property var panelVariables: predictorView ? predictionRoles.panelVariables : contourRoles.panelVariables
	readonly property var groupedPredictors: lineVariables.concat(panelVariables)
	readonly property var assignedPredictors: axisVariables.concat(groupedPredictors)
	readonly property var assignedCovariates: covariateNames.filter(function(v) { return modelPredictors.indexOf(v) >= 0 && groupedPredictors.indexOf(v) >= 0; })
	readonly property var unassignedPredictors: predictionsRoot.modelPredictors.filter(function(v) { return predictionsRoot.assignedPredictors.indexOf(v) < 0; })
	readonly property var lineCovariates: assignedCovariates.filter(function(v) { return lineVariables.indexOf(v) >= 0; })
	readonly property bool timeAxis: !variableInput || (predictionsRoot.predictorView && predictionRoles.axisIsTime)
	readonly property bool fullTimeGrid: predictionPlotType.value === "standard" && (predictionRoles.axisIsTime || (predictionRoles.axisVariables.length === 0 && predictionRoles.automaticAssignments))
	readonly property bool predictorView: !variableInput || predictionPlotType.value === "standard"
	readonly property bool lifetimeSelected: survivalTime.checked || (relativeSurvivalTime.checked && relativeSurvivalTime.enabled)
	readonly property bool categoricalAxis: predictionsRoot.predictorView && predictionRoles.axisIsCategorical
	readonly property bool probabilitySelected: survivalProbability.checked || failureProbability.checked
	readonly property bool scalarLifetimeSelected: lifetimeMean.checked || lifetimeMedian.checked
	readonly property bool lifetimeSummarySelected: lifetimePercentiles.checked || scalarLifetimeSelected
	readonly property bool lifetimePredictionSelected: survivalTime.checked && lifetimeSummarySelected
	readonly property bool predictorPlotSelected: modelPlot.checked && predictorView
	readonly property bool kaplanMeierProfilesAvailable: kaplanMeierEligibility.count > 0
	readonly property bool timeMeasuresSelected: probabilitySelected || hazard.checked || cumulativeHazard.checked || restrictedMeanSurvivalTime.checked
	readonly property string timeValuesTitle:
	{
		if (!restrictedMeanSurvivalTime.checked)
			return qsTr("Time");
		return probabilitySelected || hazard.checked || cumulativeHazard.checked ? qsTr("Time / horizon") : qsTr("Horizon");
	}
	readonly property bool plotSelected: modelPlot.checked || extraPlotSelected
	readonly property bool predictionOutputSelected: modelPlot.checked || modelTable.checked || (predictionInput.value === "dataset" && predictionExport.checked)
	readonly property bool legendPaletteAvailable: plotSelected
	readonly property bool predictionIntervalAvailable: survivalTime.checked && (variableInput ?
		lifetimeSummarySelected && predictionOutputSelected && !timeAxis && predictorView :
		modelPlot.checked && scalarLifetimeSelected)
	property string quantileStepsInfo: qsTr("Choose how to specify lifetime percentiles: equal intervals, a sequence or custom values.")
	property string quantileNumberInfo: qsTr("Set the number of equal intervals between 0 and 100.")
	property string lifeTimeStepsInfo: qsTr("Choose how to specify prediction times: equal intervals, a sequence or custom values.")
	property string lifeTimeSizeInfo: qsTr("Set the time increment. Leave blank to use the default increment.")
	property string lifeTimeCustomInfo: qsTr("Enter non-negative prediction times, separated by commas. Leave blank to use the default times.")
	title: qsTr("Predictions")

	onFullTimeGridChanged:
	{
		if (!variableInput || !analysisForm || !analysisForm.initialized() || !predictionRoles || !percentileGrid || !timeGrid || predictionRoles.applying || !percentileGrid.initialized || !timeGrid.initialized) return;
		percentileGrid.currentIndex = fullTimeGrid ? 0 : 2;
		timeGrid.currentIndex = fullTimeGrid ? 0 : 2;
	}

	AvailableVariablesList
	{
		id: kaplanMeierEligibility
		name: "predictionsKaplanMeierEligibility"
		visible: false
		source: [{rSource: "kaplanMeierAvailability"}]
	}

	Group
	{
		Group
		{
			title: qsTr("Outcome")
			CheckBox
			{
				id: survivalTime
				name: "predictionsOutcomeSurvivalTime"
				label: qsTr("Survival time")
				checked: false
				info: qsTr("Predict lifetime percentiles, median or mean from the fitted model.")
			}
			CheckBox
			{
				id: relativeSurvivalTime
				name: "predictionsOutcomeRelativeSurvivalTime"
				label: qsTr("Relative survival time")
				checked: false
				enabled: predictionsRoot.variableInput && predictionsRoot.predictorView && !predictionsRoot.timeAxis
				info: qsTr("Show the reference lifetime divided by the predicted lifetime at each predictor value.")
			}
			CheckBox
			{
				id: survivalProbability
				name: "predictionsOutcomeSurvivalProbability"
				label: qsTr("Survival probability")
				checked: false
				info: qsTr("Predict the probability of surviving beyond each selected time.")
			}
			CheckBox
			{
				id: failureProbability
				name: "predictionsOutcomeFailureProbability"
				label: qsTr("Failure probability")
				checked: false
				info: qsTr("Predict the probability of the event occurring by each selected time.")
			}
			CheckBox
			{
				id: hazard
				name: "predictionsOutcomeHazard"
				label: qsTr("Hazard")
				checked: false
				info: qsTr("Predict the event rate at each selected time.")
			}
			CheckBox
			{
				id: cumulativeHazard
				name: "predictionsOutcomeCumulativeHazard"
				label: qsTr("Cumulative hazard")
				checked: false
				info: qsTr("Predict the accumulated hazard up to each selected time.")
			}
			CheckBox
			{
				id: restrictedMeanSurvivalTime
				name: "predictionsOutcomeRestrictedMeanSurvivalTime"
				label: qsTr("Restricted mean survival time")
				checked: false
				info: qsTr("Predict the average survival time up to each selected time.")
			}
		}

		DropDown
		{
			label: qsTr("Predict for")
			id: predictionInput
			name: "predictionsPredictFor"
			startValue: "variables"
			fieldWidth: 150 * jaspTheme.uiScale
			info: qsTr("Choose predictions for selected variable values, manually entered observations, dataset rows with missing outcomes, or observations from a CSV file.")
			values:
			[
			{ value: "variables", 	label: qsTr("Variables")	 		},
			{ value: "manual", 		label: qsTr("Manual input")			},
			{ value: "dataset", 	label: qsTr("Missing outcomes (dataset)") },
			{ value: "csv", 		label: qsTr("CSV file") 				}
			]
		}

		FileSelector
		{
			name: "predictionsCsvFile"
			label: qsTr("CSV file")
			save: false
			filter: "*.csv"
			fieldWidth: 150 * jaspTheme.uiScale
			visible: predictionInput.value === "csv"
			info: qsTr("Select a CSV file with one observation per row. Column names and factor levels must match the dataset.")
		}
	}

	Group
	{
		Group
		{

			Group
			{
				title: qsTr("Lifetime")
				enabled: predictionsRoot.lifetimeSelected && predictionsRoot.predictionOutputSelected

				Group
				{
					columns:	3

					CheckBox
					{
						id: lifetimeMean
						name: "predictionsLifetimeMean"
						label: qsTr("Mean")
						checked: false
						info: qsTr("Predict the expected survival time.")
					}
					CheckBox
					{
						id: lifetimeMedian
						name: "predictionsLifetimeMedian"
						label: qsTr("Median")
						checked: false
						info: qsTr("Predict the median survival time (the 50th percentile).")
					}
					CheckBox
					{
						id: lifetimePercentiles
						name: "predictionsLifetimePercentiles"
						label: qsTr("Percentiles")
						checked: true
						info: qsTr("Predict survival times at the selected percentiles.")
					}
				}
				Group
				{
					visible: lifetimePercentiles.checked
					DropDown
					{
						name:		"predictionsLifetimePercentilesGrid"
						id:			percentileGrid
						label:		qsTr("Grid")
						info: predictionsRoot.quantileStepsInfo
						startValue: predictionsRoot.fullTimeGrid ? "quantiles" : "custom"
						depends: ["predictionsXAxisVariables", "predictionsSeparateLines", "predictionsSeparatePlots", "predictionsLifetimeTimeId", "predictionsRolesAutomatic", "predictionsOutputPlotType"]
						values:
						[
						{ label: qsTr("Equal intervals"),	value: "quantiles"	},
						{ label: qsTr("Sequence"),			value: "sequence"	},
						{ label: qsTr("Custom"),			value: "custom"		}
						]
					}
					IntegerField
					{
						name:			"predictionsLifetimePercentilesIntervals"
						label:			qsTr("Intervals")
						defaultValue:	10
						min:			1
						visible:		percentileGrid.value === "quantiles"
						info: predictionsRoot.quantileNumberInfo
					}
					Group
					{
						columns: 3
						alignFields: false
						visible: percentileGrid.value === "sequence"
						FormulaField
						{
							name:			"predictionsLifetimePercentilesFrom"
							id:				predictionsSurvivalTimeStepsFrom
							label:			qsTr("From")
							defaultValue: 	""
							inputType: 		"string"
							parseDefaultValue: false
							placeholderText: qsTr("0")
							fieldWidth: 	50 * jaspTheme.uiScale
							info: qsTr("Set the first percentile in the sequence. Leave blank to start at 0.")
						}
						FormulaField
						{
							name:			"predictionsLifetimePercentilesStep"
							id:				predictionsSurvivalTimeStepsSize
							label:			qsTr("Step")
							defaultValue: 	""
							inputType: 		"string"
							parseDefaultValue: false
							placeholderText: qsTr("10")
							fieldWidth: 	50 * jaspTheme.uiScale
							info: qsTr("Set the percentile increment. Leave blank to use an increment of 10.")
						}
						FormulaField
						{
							name:			"predictionsLifetimePercentilesTo"
							id:				predictionsSurvivalTimeStepsTo
							label:			qsTr("To")
							defaultValue: 	""
							inputType: 		"string"
							parseDefaultValue: false
							placeholderText: qsTr("100")
							fieldWidth: 	50 * jaspTheme.uiScale
							info: qsTr("Set the last percentile in the sequence. Leave blank to end at 100.")
						}
					}
					FormulaField
					{
						name:			"predictionsLifetimePercentilesValues"
						label:			qsTr("Values")
						visible:		percentileGrid.value === "custom"
						defaultValue:	"25, 50, 75"
						inputType: "string"
						parseDefaultValue: false
						info: qsTr("Enter lifetime percentiles from 0 to 100, separated by commas.")
					}
				}
			}
			Group
			{
				title: predictionsRoot.timeValuesTitle
				enabled: (predictionsRoot.timeMeasuresSelected && predictionsRoot.predictionOutputSelected) || predictionsRoot.componentPlotSelected
				DropDown
				{
					name:		"predictionsTimeGrid"
					id:			timeGrid
					label:		qsTr("Grid")
					info: 		predictionsRoot.lifeTimeStepsInfo
					startValue: predictionsRoot.fullTimeGrid ? "quantiles" : "customRelative"
					depends: ["predictionsXAxisVariables", "predictionsSeparateLines", "predictionsSeparatePlots", "predictionsLifetimeTimeId", "predictionsRolesAutomatic", "predictionsOutputPlotType"]
					values:
					[
					{ label: qsTr("Equal intervals"),	value: "quantiles"	},
					{ label: qsTr("Sequence"),			value: "sequence"	},
					{ label: qsTr("Custom (relative)"),	value: "customRelative" },
					{ label: qsTr("Custom (absolute)"),	value: "customAbsolute" }
					]
				}
				IntegerField
				{
					name:			"predictionsTimeIntervals"
					label:			qsTr("Intervals")
					defaultValue:	10
					min:			1
					visible:		timeGrid.value === "quantiles"
					info: qsTr("Set the number of equal intervals between zero and the maximum follow-up time.")
				}
				Group
				{
					columns: 3
					alignFields: false
					visible: timeGrid.value === "sequence"

					FormulaField
					{
						name:			"predictionsTimeFrom"
						id:				predictionsLifeTimeStepsFrom
						label:			qsTr("From")
						defaultValue: 	""
						inputType: 		"string"
						parseDefaultValue: false
						placeholderText: qsTr("0")
						fieldWidth:		50 * jaspTheme.uiScale
						info: qsTr("Set the first time in the sequence. Leave blank to start at 0.")
					}
					FormulaField
					{
						name:			"predictionsTimeStep"
						id:				predictionsLifeTimeStepsSize
						label:			qsTr("Step")
						defaultValue: 	""
						inputType: 		"string"
						parseDefaultValue: false
						placeholderText: qsTr("Maximum follow-up / 10")
						fieldWidth:		50 * jaspTheme.uiScale
						info: qsTr("Set the time increment. Leave blank to use the default increment.")
					}
					FormulaField
					{
						name:			"predictionsTimeTo"
						id:				predictionsLifeTimeStepsTo
						label:			qsTr("To")
						defaultValue: 	""
						inputType: 		"string"
						parseDefaultValue: false
						placeholderText: qsTr("Maximum follow-up")
						fieldWidth:		50 * jaspTheme.uiScale
						info: qsTr("Set the last time in the sequence. Leave blank to use the maximum follow-up time.")
					}
				}
				FormulaField
				{
					name: "predictionsTimePercentages"
					label: qsTr("Percentages")
					defaultValue: "25, 50, 75"
					inputType: "string"
					parseDefaultValue: false
					fieldWidth: 150 * jaspTheme.uiScale
					visible: timeGrid.value === "customRelative"
					info: qsTr("Enter prediction times as percentages of the maximum follow-up time, separated by commas.")
				}
				FormulaField
				{
					name:			"predictionsTimeValues"
					label:			qsTr("Values")
					visible:		timeGrid.value === "customAbsolute"
					inputType: "string"
					parseDefaultValue: false
					placeholderText: qsTr("25%, 50%, 75% of follow-up")
					fieldWidth: 180 * jaspTheme.uiScale
					info: predictionsRoot.lifeTimeCustomInfo
				}
			}
		}

		Group
		{
			title: qsTr("Output")
			CheckBox
			{
				id: modelPlot
				name: "predictionsOutputPlot"
				label: qsTr("Plot")
				checked: true
				childrenOnSameRow:	true
				info: qsTr("Plot every selected measure in its own panel. For new observations, use observation numbers on the x-axis and separate lines for times or lifetime percentiles.")

				DropDown
				{
					id: predictionPlotType
					visible: predictionsRoot.variableInput
					name: "predictionsOutputPlotType"
					label: ""
					startValue: "standard"
					values: [{label: qsTr("Curves and points"), value: "standard"}, {label: qsTr("Contour"), value: "contour"}]
					info: qsTr("Choose predictor plots for individual predictors, or contour plots for two numeric predictors.")
				}
			}
			CheckBox
			{
				id: modelTable
				name: "predictionsOutputTable"
				label: qsTr("Table")
				checked: false
				info: qsTr("Show a table of the selected predictions.")
			}

			CheckBox
			{
				id: predictionExport
				name: "predictionsOutputExportPredictionsToDataset"
				label: qsTr("Export predictions to dataset")
				checked: false
				visible: predictionInput.value === "dataset"
				info: qsTr("Add the selected predictions as new columns in the dataset.")
				TextField
				{
					name: "predictionsOutputExportPredictionsToDatasetColumnPrefix"
					label: qsTr("Column prefix")
					defaultValue: "Predicted"
					fieldWidth: 150 * jaspTheme.uiScale
					info: qsTr("Set a prefix for the names of exported prediction columns.")
				}
			}
		}
	}

	ParametricPredictionRoles
	{
		id: predictionRoles
		visible: predictionsRoot.variableInput && predictionsRoot.predictorView
		Layout.columnSpan: 2
		analysisForm: predictionsRoot.analysisForm
		predictors: predictionsRoot.modelPredictors
		covariates: predictionsRoot.covariateNames
		factors: predictionsRoot.factorNames
		sourcesReady: predictionsRoot.predictorSourcesReady
	}
	ParametricContourRoles
	{
		id: contourRoles
		Layout.columnSpan: 2
		visible: predictionsRoot.variableInput && !predictionsRoot.predictorView
		predictors: predictionsRoot.modelPredictors
		covariates: predictionsRoot.covariateNames
		factors: predictionsRoot.factorNames
	}
	ParametricPredictionData
	{
		name: "predictionsManualInput"
		cornerText: ""
		rowNames: Array.from({length: tableView.rowCount}, function(value, index) { return qsTr("Observation %1").arg(index + 1); })
		Layout.columnSpan: 2
		Layout.fillWidth: true
		predictionPredictors: predictionsRoot.predictionPredictors
		factorPredictors: predictionsRoot.predictionPredictors.filter(function(v) { return predictionsRoot.covariateNames.indexOf(v) < 0; })
		predictorsSourceReady: predictionsRoot.predictorSourcesReady
		visible: predictionInput.value === "manual"
	}

	DropDown
	{
		id: conditioning
		visible: predictionsRoot.variableInput
		name: "predictionsUnassignedReferenceValues"
		label: qsTr("Unassigned reference values")
		Layout.columnSpan: 2
		enabled: predictionsRoot.unassignedPredictors.length > 0
		startValue: "average"
		values: [{label: qsTr("Average (design)"), value: "average"}, {label: qsTr("Average (population)"), value: "population"}, {label: qsTr("Default"), value: "reference"}, {label: qsTr("Custom"), value: "custom"}]
		info: qsTr("Choose a design average, a population average, default reference values or custom values.")
	}
	ParametricPredictionData
	{
		name: "predictionsUnassignedReferenceValuesCustomValues"
		singleRow: true
		cornerText: ""
		rowNames: [qsTr("Value")]
		Layout.columnSpan: 2
		Layout.fillWidth: true
		predictionPredictors: predictionsRoot.unassignedPredictors
		factorPredictors: predictionsRoot.factorNames
		predictorsSourceReady: predictionsRoot.predictorSourcesReady
		visible: predictionsRoot.variableInput && conditioning.enabled && conditioning.value === "custom"
		info: qsTr("Enter reference values for unassigned predictors. Leave cells empty to use design averages.")
	}
	Group
	{
		visible: predictionsRoot.variableInput
		enabled: predictionsRoot.assignedCovariates.length > 0
		Layout.columnSpan: 2

		DropDown
		{
			id: assignedValues
			name: "predictionsAssignedCovariateValues"
			label: qsTr("Assigned covariate values")
			startValue: "quantiles"
			values: [{label: qsTr("Quantiles"), value: "quantiles"}, {label: qsTr("Custom"), value: "custom"}]
			info: qsTr("Choose quantile groups or custom values for numeric predictors used in separate lines or plots.")
		}

		IntegerField
		{
			name: 			"predictionsAssignedCovariateValuesQuantiles"
			label:			qsTr("Quantiles")
			defaultValue:	3
			min:		 	1
			visible: 		assignedValues.value === "quantiles"
			info: 			qsTr("Set the number of quantile groups for numeric predictors.")
		}

		CheckBox
		{
			name: "predictionsAssignedCovariateValuesGroupUniqueLevels"
			label: qsTr("Group unique levels")
			childrenOnSameRow: true
			checked: true
			visible: assignedValues.value === "quantiles"
			enabled: predictionsRoot.assignedCovariates.length > 0
			info: qsTr("Use individual values instead of quantile groups when the number of distinct values is within the specified limit.")

			IntegerField
			{
				name: 			"predictionsAssignedCovariateValuesGroupUniqueLevelsLimit"
				label: 			""
				defaultValue:	5
				min: 			1
			}
		}
	}

	ParametricPredictionData
	{
		name: "predictionsAssignedCovariateValuesCustomValues"
		cornerText: ""
		rowNames: Array.from({length: tableView.rowCount}, function(value, index) { return qsTr("Level %1").arg(index + 1); })
		Layout.columnSpan: 2
		Layout.fillWidth: true
		predictionPredictors: predictionsRoot.assignedCovariates
		predictorsSourceReady: predictionsRoot.predictorSourcesReady
		buttonAddText: qsTr("Add row")
		buttonDeleteText: qsTr("Remove row")
		visible: predictionsRoot.variableInput && assignedValues.value === "custom" && predictionsRoot.assignedCovariates.length > 0
		info: qsTr("Enter values for each numeric predictor used in separate lines or plots. Their combinations define the prediction groups.")
	}


	Group
	{
		DropDown
		{
			name: "predictionsSurvivalTimeYAxisScale"
			visible: predictionsRoot.predictorView
			label: qsTr("Survival time y-axis scale")
			startValue: "none"
			enabled: predictionsRoot.predictorPlotSelected && predictionsRoot.lifetimeSelected
			values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}]
			info: qsTr("Choose the y-axis scale for predicted survival time or relative survival time.")
		}
		DropDown
		{
			name: "predictionsProbabilityYAxisScale"
			visible: predictionsRoot.predictorView
			label: qsTr("Probability y-axis scale")
			startValue: "none"
			enabled: predictionsRoot.predictorPlotSelected && predictionsRoot.probabilitySelected
			values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}, {label: qsTr("Log(-log(1-p))"), value: "logmlogmp"}]
			info: qsTr("Choose the y-axis scale for survival or failure probabilities.")
		}
		DropDown
		{
			name: "predictionsXAxisTransformation"
			label: qsTr("X-axis transformation")
			visible: predictionsRoot.variableInput && predictionsRoot.predictorView && predictionsRoot.timeAxis
			enabled: modelPlot.checked && (predictionsRoot.timeMeasuresSelected || (survivalTime.checked && lifetimePercentiles.checked))
			startValue: "none"
			values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}]
			info: qsTr("Choose the x-axis scale for lifetime percentiles or time.")
		}

		Group
		{
			title: qsTr("Covariate x-axis settings")
			visible: predictionsRoot.variableInput && predictionsRoot.covariateNames.some(function(v) { return predictionsRoot.predictorView ? predictionRoles.axisVariables.indexOf(v) >= 0 : contourRoles.xVariable === v; })

			ComponentsList
			{
				name: "predictionsCovariateXAxisSettings"
				preferredWidth: implicitWidth
				optionKey: "variable"
				source: [{name: "covariates", discard: [{values: predictionsRoot.covariateNames.filter(function(v) { return predictionRoles.axisVariables.indexOf(v) < 0; })}]}]
				visible: predictionsRoot.predictorView && !predictionsRoot.timeAxis
				enabled: modelPlot.checked
				addItemManually: false
				addBorder: false
				rowComponent: ParametricPredictionAxis { title: rowLabel }
				info: qsTr("Set the axis transformation and plotting range for each numeric predictor on the x-axis.")
			}
			ParametricPredictionAxis
			{
				optionPrefix: "predictionsCovariateXAxisSettingsXAxis"
				axisLabel: qsTr("X-axis")
				visible: !predictionsRoot.predictorView
				enabled: modelPlot.checked && contourRoles.xVariable !== ""
			}
			ParametricPredictionAxis
			{
				optionPrefix: "predictionsCovariateXAxisSettingsYAxis"
				axisLabel: qsTr("Y-axis")
				visible: !predictionsRoot.predictorView
				enabled: modelPlot.checked && contourRoles.yVariable !== ""
			}
		}

		Group
		{
			title: qsTr("Contours")

			visible: predictionsRoot.variableInput && !predictionsRoot.predictorView
			enabled: modelPlot.checked
			Group
			{
				columns: 2
				alignFields: false
				DropDown
				{
					id: contourLevels
					name: "predictionsContoursLevels"
					label: qsTr("Levels")
					startValue: "number"
					values: [{label: qsTr("Number"), value: "number"}, {label: qsTr("Custom"), value: "custom"}]
					info: qsTr("Choose automatic contour levels or enter custom levels for each selected measure.")

				}

				IntegerField
				{
					name: "predictionsContoursLevelsNumber"
					label: ""
					defaultValue: 5
					min: 1
					visible: contourLevels.value === "number"
					info: qsTr("Set the approximate number of rounded contour levels.")
				}
			}

			CheckBox
			{
				name: "predictionsContoursCommonAcrossFigures"
				label: qsTr("Common across figures")
				checked: false
				visible: contourLevels.value === "number"
				info: qsTr("Use the same contour levels across figures for each selected measure.")
			}
			TextField
			{
				name: "predictionsContoursLevelsSurvivalTime"
				label: qsTr("Survival time")
				visible: contourLevels.value === "custom" && survivalTime.checked
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
			TextField
			{
				name: "predictionsContoursLevelsRelativeSurvivalTime"
				label: qsTr("Relative survival time")
				visible: contourLevels.value === "custom" && (relativeSurvivalTime.checked && relativeSurvivalTime.enabled)
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
			TextField
			{
				name: "predictionsContoursLevelsSurvivalProbability"
				label: qsTr("Survival probability")
				visible: contourLevels.value === "custom" && survivalProbability.checked
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
			TextField
			{
				name: "predictionsContoursLevelsFailureProbability"
				label: qsTr("Failure probability")
				visible: contourLevels.value === "custom" && failureProbability.checked
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
			TextField
			{
				name: "predictionsContoursLevelsHazard"
				label: qsTr("Hazard")
				visible: contourLevels.value === "custom" && hazard.checked
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
			TextField
			{
				name: "predictionsContoursLevelsCumulativeHazard"
				label: qsTr("Cumulative hazard")
				visible: contourLevels.value === "custom" && cumulativeHazard.checked
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
			TextField
			{
				name: "predictionsContoursLevelsRestrictedMeanSurvivalTime"
				label: qsTr("Restricted mean survival time")
				visible: contourLevels.value === "custom" && restrictedMeanSurvivalTime.checked
				defaultValue: ""
				inputType: "string"
				validator: null
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter contour levels in the outcome's original units, separated by commas.")
			}
		}
		Group
		{
			title: qsTr("Heatmap scale")
			visible: predictionsRoot.variableInput && !predictionsRoot.predictorView
			enabled: modelPlot.checked && contourRoles.lineVariables.length === 0
			info: qsTr("Set the colour scale and limits of the heatmap for each selected measure.")
			ParametricContourHeatmap
			{
				title: qsTr("Survival time")
				optionPrefix: "predictionsHeatmapScaleSurvivalTime"
				visible: survivalTime.checked
			}
			ParametricContourHeatmap
			{
				title: qsTr("Relative survival time")
				optionPrefix: "predictionsHeatmapScaleRelativeSurvivalTime"
				visible: relativeSurvivalTime.checked && relativeSurvivalTime.enabled
			}
			ParametricContourHeatmap
			{
				title: qsTr("Survival probability")
				optionPrefix: "predictionsHeatmapScaleSurvivalProbability"
				visible: survivalProbability.checked
			}
			ParametricContourHeatmap
			{
				title: qsTr("Failure probability")
				optionPrefix: "predictionsHeatmapScaleFailureProbability"
				visible: failureProbability.checked
			}
			ParametricContourHeatmap
			{
				title: qsTr("Hazard")
				optionPrefix: "predictionsHeatmapScaleHazard"
				visible: hazard.checked
			}
			ParametricContourHeatmap
			{
				title: qsTr("Cumulative hazard")
				optionPrefix: "predictionsHeatmapScaleCumulativeHazard"
				visible: cumulativeHazard.checked
			}
			ParametricContourHeatmap
			{
				title: qsTr("Restricted mean survival time")
				optionPrefix: "predictionsHeatmapScaleRestrictedMeanSurvivalTime"
				visible: restrictedMeanSurvivalTime.checked
			}
		}
		IntegerField
		{
			name: "predictionsPredictorValuesInTable"
			label: qsTr("Predictor values in table")
			defaultValue: 10
			min: 2
			visible: predictionsRoot.variableInput && predictionsRoot.axisVariables.length > 0 && !predictionsRoot.timeAxis && !predictionsRoot.categoricalAxis && modelTable.checked
			info: qsTr("Set the number of numeric predictor values used in prediction tables.")
		}
	}

	Group
	{
		title: qsTr("Display")

		Group
		{
			CheckBox
			{
				label:				qsTr("Confidence intervals")
				name:				"predictionsDisplayConfidenceIntervals"
				enabled:			predictionsRoot.predictionOutputSelected
				checked:			true
				childrenOnSameRow:	true
				info: qsTr("Include confidence intervals for the figures and tables.")
				CIField
				{
					name: "predictionsDisplayConfidenceIntervalsLevel"
					info: qsTr("Set the confidence level for the confidence intervals.")
				}
			}
			CheckBox
			{
				name: "predictionsDisplayPredictionInterval"
				label: qsTr("Prediction interval")
				checked: false
				childrenOnSameRow: true
				enabled: predictionsRoot.predictionIntervalAvailable
				info: qsTr("Show a range for individual survival times at the selected predictor values.")
				CIField
				{
					name: "predictionsDisplayPredictionIntervalLevel"
					info: qsTr("Set the coverage of the prediction interval.")
				}
			}
		}

		CheckBox
		{
			name: "predictionsDisplayCombinePredictorPlots"
			visible: predictionsRoot.variableInput
			label: qsTr("Combine predictor plots")
			checked: false
			enabled: predictionsRoot.predictorPlotSelected && !predictionsRoot.timeAxis && predictionsRoot.axisVariables.length > 1
			info: qsTr("Combine the plots for the selected predictors in one figure.")
		}
		DropDown
		{
			name:		"plotLegend"
			label:		qsTr("Legend")
			startValue:	"right"
			enabled:	predictionsRoot.legendPaletteAvailable
			info: qsTr("Choose the legend position for prediction plots.")
			values:
			[
			{ label: qsTr("Bottom"),	value: "bottom" },
			{ label: qsTr("Right"),		value: "right"	},
			{ label: qsTr("Left"),		value: "left"	},
			{ label: qsTr("Top"),		value: "top"	},
			{ label: qsTr("None"),		value: "none"	}
			]
		}
		DropDown
		{
			name:		"plotTheme"
			label:		qsTr("Theme")
			startValue:	"jasp"
			enabled:	predictionsRoot.plotSelected
			info: qsTr("Choose the theme for prediction plots.")
			values:
			[
			{ label: "JASP",					value: "jasp"				},
			{ label: qsTr("White background"),	value: "whiteBackground"	},
			{ label: qsTr("Light"),				value: "light"				},
			{ label: qsTr("Minimal")	,		value: "minimal"			},
			{ label: "APA",						value: "apa"				},
			{ label: "pubr",					value: "pubr"				},
			{ label: qsTr("Detailed"),			value: "detailed"			}
			]
		}
		CheckBox
		{
			name: "predictionsDisplayConnectXAxisLevels"
			visible: predictionsRoot.variableInput
			label: qsTr("Connect x-axis levels")
			checked: true
			enabled: predictionsRoot.predictorPlotSelected && predictionsRoot.axisVariables.some(function(variable) { return predictionsRoot.factorNames.indexOf(variable) >= 0; })
			info: qsTr("Connect predicted values across factor levels on the x-axis.")
		}
		CheckBox
		{
			name: "predictionsDisplayObservedData"
			visible: predictionsRoot.variableInput
			label: qsTr("Observed data")
			checked: false
			enabled: modelPlot.checked && (predictionsRoot.predictorView ? predictionsRoot.lifetimePredictionSelected && !predictionsRoot.timeAxis : contourRoles.axes.length === 2)
			info: qsTr("Add observed event times to predictor plots, or observed predictor values to contour plots.")
		}
		CheckBox
		{
			name:		"predictionsDisplayKaplanMeier"
			visible: predictionsRoot.variableInput
			label:		qsTr("Kaplan-Meier")
			enabled:	modelPlot.checked && predictionsRoot.rightCensoring && predictionsRoot.timeAxis && predictionsRoot.probabilitySelected && predictionsRoot.kaplanMeierProfilesAvailable
			info: qsTr("Overlay Kaplan-Meier curves for comparison with the model predictions, where available.")
		}
		CheckBox
		{
			name:		"predictionsDisplayCensoringEvents"
			visible: predictionsRoot.variableInput
			label:		qsTr("Censoring events")
			enabled:	modelPlot.checked && predictionsRoot.rightCensoring && predictionPlotType.value === "standard" && ((predictionsRoot.timeAxis && predictionsRoot.timeMeasuresSelected) || predictionsRoot.lifetimePredictionSelected)
			info: qsTr("Show censored observations as marks along the time axis.")
		}
		ColorPalette
		{
			enabled: predictionsRoot.legendPaletteAvailable
			info: qsTr("Colors for prediction series.")
		}
	}

	Group
	{
		columns: 1
		Group
		{
			title: qsTr("Merge series across")
			visible: predictionsRoot.variableInput
			columns: 2
			CheckBox
			{
				name: "predictionsMergeSeriesAcrossDistributions"
				label: qsTr("Distributions")
				checked: false
				enabled: predictionsRoot.mergeDistributionsAvailable
				info: qsTr("Overlay selected distributions.")
			}
			CheckBox
			{
				name: "predictionsMergeSeriesAcrossSubgroups"
				label: qsTr("Subgroups")
				checked: false
				enabled: predictionsRoot.subgroupsAvailable
				info: qsTr("Overlay predictions for the separately fitted subgroups.")
			}
			CheckBox
			{
				name: "predictionsMergeSeriesAcrossComponents"
				label: qsTr("Components")
				checked: false
				visible: predictionsRoot.mixture
				enabled: predictionsRoot.mergeComponentsAvailable
				info: qsTr("Overlay selected numbers of mixture components.")
			}
		}
		CheckBox
		{
			label:		qsTr("Merge tables across measures")
			name:		"predictionsMergeTablesAcrossMeasures"
			checked:	false
			enabled: predictionsRoot.predictorView
			info: qsTr("Combine the selected time-based prediction measures in one table.")
		}

		CheckBox
		{
			name:		"predictionsRoundTimeTableValues"
			label:		qsTr("Round time table values")
			checked:	true
			visible:	timeGrid.value === "quantiles" || timeGrid.value === "sequence"
			enabled: modelTable.checked
			info: qsTr("Round prediction times in tables to the nearest integer.")
		}
	}
}
