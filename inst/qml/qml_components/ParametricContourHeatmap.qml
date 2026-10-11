pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	id: heatmapOptions
	property string optionPrefix: ""
	columns: 2
	alignFields: false
	DropDown
	{
		name: heatmapOptions.optionPrefix + "Transformation"
		label: qsTr("Transformation")
		Layout.columnSpan: 2
		startValue: "none"
		values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}]
		info: qsTr("Choose the transformation for the heatmap colour scale.")
	}
	FormulaField
	{
		name: heatmapOptions.optionPrefix + "From"
		label: qsTr("From")
		defaultValue: ""
		inputType: "string"
		parseDefaultValue: false
		fieldWidth: 65 * jaspTheme.uiScale
		info: qsTr("Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.")
	}
	FormulaField
	{
		name: heatmapOptions.optionPrefix + "To"
		label: qsTr("To")
		defaultValue: ""
		inputType: "string"
		parseDefaultValue: false
		fieldWidth: 65 * jaspTheme.uiScale
		info: qsTr("Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.")
	}
}
