pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	id: timeOptions
	property alias gridControl: timeGrid
	property string fromPlaceholder: qsTr("0")
	property string gridInfo: qsTr("Choose how to specify prediction times: equal intervals, a sequence or custom values.")
	property string intervalsInfo: qsTr("Set the number of equal intervals between zero and the maximum follow-up time.")
	property string fromInfo: qsTr("Set the first time in the sequence. Leave blank to start at 0.")
	property string stepInfo: qsTr("Set the time increment. Leave blank to use the default increment.")
	property string toInfo: qsTr("Set the last time in the sequence. Leave blank to use the maximum follow-up time.")
	property string valuesInfo: qsTr("Enter non-negative prediction times, separated by commas. Leave blank to use the default times.")
	signal gridInitializedChanged()
	title: qsTr("Time")

	DropDown
	{
		id: timeGrid
		name: "predictionsTimeGrid"
		label: qsTr("Grid")
		startValue: "quantiles"
		onInitializedChanged: timeOptions.gridInitializedChanged()
		values: [
			{label: qsTr("Equal intervals"), value: "quantiles"},
			{label: qsTr("Sequence"), value: "sequence"},
			{label: qsTr("Custom (relative)"), value: "customRelative"},
			{label: qsTr("Custom (absolute)"), value: "customAbsolute"}
		]
		info: timeOptions.gridInfo
	}
	IntegerField
	{
		name: "predictionsTimeIntervals"
		label: qsTr("Intervals")
		defaultValue: 10
		min: 1
		visible: timeGrid.value === "quantiles"
		info: timeOptions.intervalsInfo
	}
	Group
	{
		columns: 3
		alignFields: false
		visible: timeGrid.value === "sequence"
		FormulaField
		{
			name: "predictionsTimeFrom"
			label: qsTr("From")
			defaultValue: ""
			inputType: "string"
			parseDefaultValue: false
			placeholderText: timeOptions.fromPlaceholder
			fieldWidth: 50 * jaspTheme.uiScale
			info: timeOptions.fromInfo
		}
		FormulaField
		{
			name: "predictionsTimeStep"
			label: qsTr("Step")
			defaultValue: ""
			inputType: "string"
			parseDefaultValue: false
			placeholderText: qsTr("Maximum follow-up / 10")
			fieldWidth: 50 * jaspTheme.uiScale
			info: timeOptions.stepInfo
		}
		FormulaField
		{
			name: "predictionsTimeTo"
			label: qsTr("To")
			defaultValue: ""
			inputType: "string"
			parseDefaultValue: false
			placeholderText: qsTr("Maximum follow-up")
			fieldWidth: 50 * jaspTheme.uiScale
			info: timeOptions.toInfo
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
		name: "predictionsTimeValues"
		label: qsTr("Values")
		inputType: "string"
		parseDefaultValue: false
		placeholderText: qsTr("25%, 50%, 75% of follow-up")
		fieldWidth: 180 * jaspTheme.uiScale
		visible: timeGrid.value === "customAbsolute"
		info: timeOptions.valuesInfo
	}
}
