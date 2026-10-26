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
	property var analysisForm
	property var modelPredictors: []
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false
	readonly property var selectedStratifiers: stratificationSelector.selectedStratifiers
	readonly property bool continuousStrata: stratificationSelector.continuousStrata
	readonly property bool strataPossible: stratificationSelector.strataPossible
	property bool subgroupsAvailable:	false
	property bool mergeComponentsActive:	false
	property alias extraMergeControls:	mergeExtras.content
	readonly property bool subgroupsMerged: probabilityPlotMergePlotsAcrossSubgroups.checked && probabilityPlotMergePlotsAcrossSubgroups.enabled
	readonly property bool plotsMerged: (probabilityPlotMergePlotsAcrossStrata.checked && probabilityPlotMergePlotsAcrossStrata.enabled) ||
		subgroupsMerged || (probabilityPlotMergePlotsAcrossDistributions.checked && probabilityPlotMergePlotsAcrossDistributions.enabled) || mergeComponentsActive
	readonly property bool legendPaletteAvailable: plotsMerged && (probabilityPlotFittedCurve.checked ||
		probabilityPlotEmpiricalPoints.checked || (probabilityPlotCensoringEvents.checked && probabilityPlotCensoringEvents.enabled))

	name:		"probabilityPlot"
	label:		qsTr("Probability plot")
	enabled:	probabilityRoot.rightCensoring
	info: qsTr("Compare observed failure times with the fitted distribution to assess model fit. Available for right-censored data.")

	DropDown
	{
		name:		"probabilityPlotCanvas"
		label:		qsTr("Canvas")
		startValue:	"weibull"
		info: qsTr("Choose the reference distribution for the probability plot's axes.")
		values:
		[
			{ label: qsTr("Weibull"),		value: "weibull"},
			{ label: qsTr("Exponential"),	value: "exponential"},
			{ label: qsTr("Log-normal"),	value: "lognormal"},
			{ label: qsTr("Log-logistic"),	value: "loglogistic"}
		]
	}

	ParametricObservedStrata
	{
		id: stratificationSelector
		prefix: "probabilityPlot"
		analysisForm: probabilityRoot.analysisForm
		modelPredictors: probabilityRoot.modelPredictors
		covariateNames: probabilityRoot.covariateNames
		factorNames: probabilityRoot.factorNames
		predictorSourcesReady: probabilityRoot.predictorSourcesReady
		info: qsTr("Show separate groups of observed data for the selected predictors.")
	}

	CheckBox
	{
		name:		"probabilityPlotEmpiricalPoints"
		id:			probabilityPlotEmpiricalPoints
		label:		qsTr("Empirical points")
		checked:	true
		info: qsTr("Show points representing the observed failure probabilities.")

		CheckBox
		{
			name:		"probabilityPlotEmpiricalPointsCoordinates"
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
			name:		"probabilityPlotMergePlotsAcrossSubgroups"
			id:			probabilityPlotMergePlotsAcrossSubgroups
			label:		qsTr("Subgroups")
			checked:	false
			enabled:	probabilityRoot.subgroupsAvailable
			info: qsTr("Overlay probability plots for separately fitted subgroups.")
		}

		CheckBox
		{
			name:		"probabilityPlotMergePlotsAcrossStrata"
			id:			probabilityPlotMergePlotsAcrossStrata
			label:		qsTr("Strata")
			checked:	true
			enabled:	stratificationSelector.checked && probabilityRoot.modelPredictors.length > 0
			info: qsTr("Overlay the observed groups in one probability plot, or show each group in a separate plot.")
		}

		Group
		{
			id:		mergeExtras
			visible:	hasChildren
		}
	}

	CheckBox
	{
		name:				"probabilityPlotConfidenceIntervals"
		label:				qsTr("Confidence intervals")
		checked:			true
		childrenOnSameRow:	true
		info: qsTr("Include confidence interval bounds for the fitted probability curve.")

		CIField
		{
			name:			"probabilityPlotConfidenceIntervalsLevel"
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
		info: qsTr("Choose how to adjust the ranks of observations when some survival times are censored.")
		values:
		[
			{ label: qsTr("Johnson"),		value: "johnson"},
			{ label: qsTr("Kaplan-Meier"),	value: "kaplanMeier"}
		]
	}

	DropDown
	{
		name:		"probabilityPlotTies"
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
		info: qsTr("Choose the legend position for the probability plot.")
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
		info: qsTr("Choose the colours used in the probability plot.")
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
