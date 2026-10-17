pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	id: axisOptions
	property string optionPrefix: ""
	property string axisLabel: qsTr("X-axis")
	columns: 2
	DropDown
	{
		Layout.columnSpan: 2
		name: axisOptions.optionPrefix ? axisOptions.optionPrefix + "Transformation" : "xAxisTransformation"
		label: qsTr("%1 transformation").arg(axisOptions.axisLabel)
		startValue: "none"
		values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}, {label: qsTr("Model term"), value: "modelTerm"}]
		info: qsTr("Choose the axis transformation: the original scale, a logarithmic scale or the transformation used in the model.")
	}
	FormulaField
	{
		name: axisOptions.optionPrefix ? axisOptions.optionPrefix + "From" : "from"
		alignInGroup: false
		label: qsTr("From")
		defaultValue: ""
		inputType: "string"
		parseDefaultValue: false
		fieldWidth: 65 * jaspTheme.uiScale
		info: qsTr("Set the first predictor value to display, in original units. Leave blank to use the observed minimum.")
	}
	FormulaField
	{
		name: axisOptions.optionPrefix ? axisOptions.optionPrefix + "To" : "to"
		label: qsTr("To")
		defaultValue: ""
		inputType: "string"
		parseDefaultValue: false
		fieldWidth: 65 * jaspTheme.uiScale
		info: qsTr("Set the last predictor value to display, in original units. Leave blank to use the observed maximum.")
	}
}
