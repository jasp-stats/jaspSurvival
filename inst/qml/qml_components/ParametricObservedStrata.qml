pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

CheckBox
{
	id: strataRoot
	property string prefix
	property var analysisForm
	property var modelPredictors: []
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false
	property var selectedStratifiers: []
	readonly property bool continuousStrata: selectedStratifiers.some(function(v) { return covariateNames.indexOf(v) >= 0 && modelPredictors.indexOf(v) >= 0; })
	readonly property bool strataPossible: checked && selectedStratifiers.some(function(v) { return modelPredictors.indexOf(v) >= 0; })
	name: prefix + "StratifyBy"
	label: qsTr("Stratify by")
	checked: true
	visible: modelPredictors.length > 0
	info: qsTr("Show separate groups of observed data for the selected predictors.")
	function queueStratification() { Qt.callLater(refreshStratification); }
	function refreshStratification()
	{
		if (!predictorSourcesReady || !analysisForm || !analysisForm.initialized() || !stratification.initialized) return;
		let rows = JSON.parse(JSON.stringify(analysisForm.options()[stratification.name]));
		// Native keys gain value/type metadata when row options change.
		let selected = rows.filter(function(row) { return row.stratify; }).map(function(row) {
			return typeof row.variable === "string" ? row.variable : row.variable.value;
		});
		if (selected.length !== selectedStratifiers.length || selected.some(function(v, i) { return v !== selectedStratifiers[i]; }))
			selectedStratifiers = selected;
	}
	property Connections analysisConnections: Connections
	{
		target: strataRoot.analysisForm
		function onAnalysisChanged() { strataRoot.queueStratification(); }
	}
	onPredictorSourcesReadyChanged: queueStratification()
	onModelPredictorsChanged: queueStratification()
	ComponentsList
	{
		id: stratification
		name: strataRoot.prefix + "StratifyByVariables"
		optionKey: "variable"
		addItemManually: false
		addBorder: true
		source: [
			{name: "covariates", discard: [{values: strataRoot.covariateNames.filter(function(v) { return strataRoot.modelPredictors.indexOf(v) < 0; })}]},
			{name: "factors", discard: [{values: strataRoot.factorNames.filter(function(v) { return strataRoot.modelPredictors.indexOf(v) < 0; })}]}
		]
		onCountChanged: strataRoot.queueStratification()
		onInitializedChanged: strataRoot.queueStratification()
		rowComponent: CheckBox
		{
			name: "stratify"
			label: rowLabel
			checked: strataRoot.factorNames.indexOf(rowValue) >= 0
			onCheckedChanged: strataRoot.queueStratification()
		}
		info: qsTr("Choose the predictors used to group observations. Factors use their levels; numeric predictors use quantile groups.")
	}
	IntegerField
	{
		name: strataRoot.prefix + "StratifyByQuantiles"
		label: qsTr("Quantiles")
		defaultValue: 3
		min: 1
		enabled: strataRoot.checked
		info: qsTr("Set the number of quantile groups for numeric predictors.")
	}
	CheckBox
	{
		name: strataRoot.prefix + "StratifyByGroupUniqueLevels"
		label: qsTr("Group unique levels")
		checked: true
		childrenOnSameRow: true
		enabled: strataRoot.checked
		IntegerField
		{
			name: strataRoot.prefix + "StratifyByGroupUniqueLevelsLimit"
			defaultValue: 5
			min: 1
			label: ""
		}
		info: qsTr("Use individual values instead of quantile groups when the number of distinct values is within the specified limit.")
	}
}
