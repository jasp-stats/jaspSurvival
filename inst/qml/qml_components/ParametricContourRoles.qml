pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	id: contourRoles
	property var predictors: []
	property var covariates: []
	property var factors: []
	property bool sourcesReady: false
	readonly property bool continuousGroups: contourLines.columnsTypes.indexOf("scale") >= 0 || contourPanels.columnsTypes.indexOf("scale") >= 0
	readonly property string xVariable: contourX.count ? contourX.columnsNames[0] : ""
	readonly property string yVariable: contourY.count ? contourY.columnsNames[0] : ""
	readonly property var axes: [xVariable, yVariable].filter(function(v) { return v !== ""; })
	readonly property var lineVariables: names(contourLines.columnsNames)
	readonly property var panelVariables: names(contourPanels.columnsNames)
	function names(sequence)
	{
		let result = [];
		for (let i = 0; i < sequence.length; ++i) result.push(sequence[i]);
		return result;
	}
	readonly property var typedSources: [
		{name: "covariates", discard: [{values: covariates.filter(function(v) { return sourcesReady && predictors.indexOf(v) < 0; })}]},
		{name: "factors", discard: [{values: factors.filter(function(v) { return sourcesReady && predictors.indexOf(v) < 0; })}]}
	]
	VariablesForm
	{
		preferredHeight: 310 * jaspTheme.uiScale
		AvailableVariablesList
		{
			name: "predictionsContourModelVariables"
			title: qsTr("Model variables")
			source: contourRoles.typedSources
		}
		AssignedVariablesList
		{
			id: contourX
			name: "predictionsContourXAxis"
			title: qsTr("X-axis")
			singleVariable: true
			allowedColumns: ["scale"]
			allowTypeChange: false
			info: qsTr("Select the numeric predictor for the horizontal contour axis.")
		}
		AssignedVariablesList
		{
			id: contourY
			name: "predictionsContourYAxis"
			title: qsTr("Y-axis")
			singleVariable: true
			allowedColumns: ["scale"]
			allowTypeChange: false
			info: qsTr("Select the numeric predictor for the vertical contour axis.")
		}
		AssignedVariablesList
		{
			id: contourLines
			name: "predictionsContourSeparateLines"
			title: qsTr("Separate lines")
			allowedColumns: ["scale", "nominal"]
			allowTypeChange: false
			info: qsTr("Draw separate contour lines for the levels or groups of the selected predictors.")
		}
		AssignedVariablesList
		{
			id: contourPanels
			name: "predictionsContourSeparatePlots"
			title: qsTr("Separate plots")
			allowedColumns: ["scale", "nominal"]
			allowTypeChange: false
			info: qsTr("Create separate contour plots for the levels or groups of the selected predictors.")
		}
	}
}
