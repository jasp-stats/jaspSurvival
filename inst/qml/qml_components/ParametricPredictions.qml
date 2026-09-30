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
	id:	predictionsRoot
	property bool rightCensoring:	false
	property bool mergeDistributionsAvailable:	false
	property bool categoricalLevelsPossible:	false
	property bool mergeTimeComponentsActive:	false
	property bool mergeLifeComponentsActive:	false
	property bool extraPlotSelected:	false
	property alias survivalTimeExtraControls:	survivalTimeExtras.content
	property alias lifeTimeExtraControls:	lifeTimeExtras.content

	readonly property bool timeSeriesAvailable: (survivalTimeMergePlotsAcrossDistributions.checked && survivalTimeMergePlotsAcrossDistributions.enabled) || mergeTimeComponentsActive || categoricalLevelsPossible
	readonly property bool lifeSeriesAvailable: (lifeTimeMergePlotsAcrossDistributions.checked && lifeTimeMergePlotsAcrossDistributions.enabled) || mergeLifeComponentsActive || categoricalLevelsPossible
	readonly property bool lifePlotSelected: survivalProbabilityPlot.checked || hazardPlot.checked || cumulativeHazardPlot.checked || restrictedMeanSurvivalTimePlot.checked
	readonly property bool plotSelected: survivalTimePlot.checked || lifePlotSelected || extraPlotSelected
	readonly property bool legendPaletteAvailable: (survivalTimePlot.checked && timeSeriesAvailable) || (lifePlotSelected && lifeSeriesAvailable) || extraPlotSelected
	property string quantileStepsInfo:	qsTr("Select the probabilities at which survival times are predicted: Quantiles, Sequence, or Custom.")
	property string quantileNumberInfo:	qsTr("Specify the number of predicted survival-time quantiles when using Quantiles as the steps type.")
	property string lifeTimeStepsInfo:	qsTr("Select the time points for predictions: Equal spacing, Sequence, or Custom. Equal spacing uses evenly spaced times up to the maximum observed time; survival plots with a logarithmic time axis use evenly spaced log times.")
	property string lifeTimeSizeInfo:	qsTr("Set the time increment when using Sequence steps. Leaving this blank uses one tenth of the selected time range.")
	property string lifeTimeCustomInfo:	qsTr("Specify custom steps of the life time.")

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
			info: predictionsRoot.quantileStepsInfo
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
			info: predictionsRoot.quantileNumberInfo
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
			enabled:	predictionsRoot.mergeDistributionsAvailable
			info: qsTr("Merge the plots for survival probability across distributions into a single plot. Only available when no model selection is being performed.")
		}

		Group
		{
			id:		survivalTimeExtras
			visible:	hasChildren
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
				enabled:	predictionsRoot.legendPaletteAvailable
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
				enabled:	predictionsRoot.legendPaletteAvailable
				info: qsTr("Customize the color palette used in prediction plots with multiple displayed curves.")
			}

			DropDown
			{
				name:		"plotTheme"
				label:		qsTr("Theme")
				startValue:	"jasp"
				enabled:	predictionsRoot.plotSelected
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
					enabled:	predictionsRoot.rightCensoring
					info: qsTr("Show a Kaplan-Meier curve in the plot. Only available when Censoring Type is set to Right.")
				}

				CheckBox
				{
					name:		"survivalProbabilityPlotCensoringEvents"
					label:		qsTr("Censoring events")
					enabled:	predictionsRoot.rightCensoring
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
			info: predictionsRoot.lifeTimeStepsInfo
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
			info: predictionsRoot.lifeTimeSizeInfo
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
			info: predictionsRoot.lifeTimeCustomInfo
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
			enabled:	predictionsRoot.mergeDistributionsAvailable
			info: qsTr("Merge the plots for survival time, survival probabilities, hazard, cumulative hazard, and restricted mean survival across distributions into a single plot. Only available when no model selection is being performed.")
		}

		Group
		{
			id:		lifeTimeExtras
			visible:	hasChildren
		}

	}

}
