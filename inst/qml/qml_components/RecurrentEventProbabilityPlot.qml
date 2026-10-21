import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

CheckBox
{
	id: probabilityRoot
	property bool hasSubgroups: false
	name: "probabilityPlot"
	label: qsTr("Probability plot")
	info: qsTr("Check Poisson interval counts using randomized quantile residuals from fitted interval means. Normal Q-Q and uniform P-P plots should follow the reference line under an NHPP. This is a count-model diagnostic, not a lifetime probability plot. Set a seed under Advanced to reproduce it.")

	DropDown
	{
		name: "probabilityPlotCanvas"
		label: qsTr("Canvas")
		startValue: "normal"
		values: [{label: qsTr("Normal (Q-Q)"), value: "normal"}, {label: qsTr("Uniform (P-P)"), value: "uniform"}]
		info: qsTr("Compare quantile residuals with standard normal quantiles, or their cumulative probabilities with uniform plotting positions.")
	}
	CheckBox
	{
		id: empiricalPoints
		name: "probabilityPlotEmpiricalPoints"
		label: qsTr("Empirical points")
		checked: true
		info: qsTr("Show one randomized quantile residual for each observation interval.")
		CheckBox
		{
			name: "probabilityPlotEmpiricalPointsCoordinates"
			label: qsTr("Coordinates")
			info: qsTr("Display the theoretical and observed coordinates beside each point.")
		}
	}
	CheckBox
	{
		name: "probabilityPlotReferenceLine"
		label: qsTr("Reference line")
		checked: true
		info: qsTr("Show the identity line expected under the fitted NHPP count model.")
	}
	Group
	{
		title: qsTr("Merge Plots Across")
		CheckBox
		{
			id: mergeSubgroups
			name: "probabilityPlotMergePlotsAcrossSubgroups"
			label: qsTr("Subgroups")
			enabled: probabilityRoot.hasSubgroups
			info: qsTr("Overlay probability diagnostics for separately fitted subgroups.")
		}
	}
	CheckBox
	{
		id: referenceEnvelope
		name: "probabilityPlotReferenceEnvelope"
		label: qsTr("Reference envelope")
		checked: true
		childrenOnSameRow: true
		info: qsTr("Show pointwise order-statistic bounds for the reference distribution, treating fitted parameters as known. This exploratory envelope does not account for parameter estimation and is not a calibrated goodness-of-fit test.")
		CIField
		{
			name: "probabilityPlotReferenceEnvelopeLevel"
			defaultValue: 90
			info: qsTr("Set the pointwise reference-envelope coverage.")
		}
	}
	CheckBox
	{
		name: "probabilityPlotGrid"
		label: qsTr("Grid")
		checked: true
		info: qsTr("Show grid lines on the probability plot.")
	}
	DropDown
	{
		name: "probabilityPlotPlottingPosition"
		label: qsTr("Plotting position")
		startValue: "median"
		values: [
			{label: qsTr("Median"), value: "median"},
			{label: qsTr("Benard"), value: "benard"},
			{label: qsTr("Hazen"), value: "hazen"},
			{label: qsTr("Mean"), value: "mean"},
			{label: qsTr("Blom"), value: "blom"}
		]
		info: qsTr("Choose the theoretical plotting positions for the ordered interval residuals.")
	}
	DropDown
	{
		name: "probabilityPlotLegend"
		label: qsTr("Legend")
		enabled: probabilityRoot.hasSubgroups && mergeSubgroups.checked && empiricalPoints.checked
		startValue: "right"
		values: [
			{label: qsTr("Bottom"), value: "bottom"}, {label: qsTr("Right"), value: "right"},
			{label: qsTr("Left"), value: "left"}, {label: qsTr("Top"), value: "top"}, {label: qsTr("None"), value: "none"}
		]
		info: qsTr("Choose the legend position for merged subgroup plots.")
	}
	ColorPalette
	{
		name: "probabilityPlotColorPalette"
		enabled: probabilityRoot.hasSubgroups && mergeSubgroups.checked && (empiricalPoints.checked || referenceEnvelope.checked)
		info: qsTr("Choose colors for subgroup probability diagnostics.")
	}
	DropDown
	{
		name: "probabilityPlotTheme"
		label: qsTr("Theme")
		startValue: "jasp"
		values: [
			{label: "JASP", value: "jasp"}, {label: qsTr("Detailed"), value: "detailed"},
			{label: qsTr("White background"), value: "whiteBackground"}, {label: qsTr("Light"), value: "light"},
			{label: qsTr("Minimal"), value: "minimal"}, {label: "APA", value: "apa"}, {label: "pubr", value: "pubr"}
		]
		info: qsTr("Choose the probability plot theme.")
	}
}
