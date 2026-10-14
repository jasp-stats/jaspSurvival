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

CheckBox
{
	id:	probabilityRoot
	property bool rightCensoring:	false
	property bool mergeDistributionsAvailable:	false
	property bool categoricalLevelsPossible:	false
	property bool subgroupsAvailable:	false
	property bool mergeComponentsActive:	false
	property alias extraMergeControls:	mergeExtras.content
	readonly property bool subgroupsMerged: probabilityPlotMergePlotsAcrossSubgroups.checked && probabilityPlotMergePlotsAcrossSubgroups.enabled
	readonly property bool legendPaletteAvailable: (probabilityPlotFittedCurve.checked && ((probabilityPlotMergePlotsAcrossDistributions.checked && probabilityPlotMergePlotsAcrossDistributions.enabled) || mergeComponentsActive || (probabilityPlotMergePlotsAcrossFactors.checked && probabilityPlotMergePlotsAcrossFactors.enabled) || subgroupsMerged)) || (((probabilityPlotMergePlotsAcrossFactors.checked && probabilityPlotMergePlotsAcrossFactors.enabled) || subgroupsMerged) && (probabilityPlotEmpiricalPoints.checked || (probabilityPlotCensoringEvents.checked && probabilityPlotCensoringEvents.enabled)))

	name:		"probabilityPlot"
	label:		qsTr("Probability plot")
	enabled:	probabilityRoot.rightCensoring
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
		id:			probabilityPlotEmpiricalPoints
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
		id:			probabilityPlotCensoringEvents
		label:		qsTr("Censoring events")
		enabled:	probabilityRoot.rightCensoring
		info: qsTr("Show censored observations as rug marks at the bottom of the plot. Only available when Censoring Type is set to Right.")
	}

	Group
	{
		title:		qsTr("Merge Plots Across")
		columns:	2

		CheckBox
		{
			name:		"probabilityPlotMergePlotsAcrossDistributions"
			id:			probabilityPlotMergePlotsAcrossDistributions
			label:		qsTr("Distributions")
			checked:	false
			enabled:	probabilityRoot.mergeDistributionsAvailable
			info: qsTr("Merge the probability plots across distributions into a single plot. Only available when no model selection is being performed.")
		}

		CheckBox
		{
			name:		"probabilityPlotMergePlotsAcrossFactors"
			id:			probabilityPlotMergePlotsAcrossFactors
			label:		qsTr("Factors")
			checked:	true
			enabled:	probabilityRoot.categoricalLevelsPossible
			info: qsTr("When checked, combine observed combinations of fitted factors in one plot. When unchecked, show a separate plot for each combination. In both cases, predictions use the unweighted mean model design (including interaction terms) within each combination.")
		}

		CheckBox
		{
			name:		"probabilityPlotMergePlotsAcrossSubgroups"
			id:			probabilityPlotMergePlotsAcrossSubgroups
			label:		qsTr("Subgroups")
			checked:	false
			enabled:	probabilityRoot.subgroupsAvailable
			info: qsTr("Overlay the separately fitted subgroup curves in one plot, retaining separate observed data for each subgroup.")
		}

		Group
		{
			id:		mergeExtras
			visible:	hasChildren
		}
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
		enabled:	probabilityRoot.legendPaletteAvailable
		info: qsTr("Choose the legend position for probability plots with multiple fitted curves or factor levels.")
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
		enabled:	probabilityRoot.legendPaletteAvailable
		info: qsTr("Customize the color palette used in probability plots with multiple fitted curves or factor levels.")
	}

	DropDown
	{
		name:		"probabilityPlotTheme"
		label:		qsTr("Theme")
		startValue:	"jasp"
		enabled:	probabilityRoot.checked
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
