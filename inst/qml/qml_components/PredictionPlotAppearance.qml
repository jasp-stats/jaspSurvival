pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import JASP.Controls
import JASP

Group
{
	id: appearance
	property bool legendAvailable: true
	property bool plotsAvailable: true
	DropDown
	{
		name: "plotLegend"
		label: qsTr("Legend")
		startValue: "right"
		enabled: appearance.legendAvailable
		values: [
			{label: qsTr("Bottom"), value: "bottom"},
			{label: qsTr("Right"), value: "right"},
			{label: qsTr("Left"), value: "left"},
			{label: qsTr("Top"), value: "top"},
			{label: qsTr("None"), value: "none"}
		]
		info: qsTr("Choose the legend position for prediction plots.")
	}
	DropDown
	{
		name: "plotTheme"
		label: qsTr("Theme")
		startValue: "jasp"
		enabled: appearance.plotsAvailable
		values: [
			{label: "JASP", value: "jasp"},
			{label: qsTr("White background"), value: "whiteBackground"},
			{label: qsTr("Light"), value: "light"},
			{label: qsTr("Minimal"), value: "minimal"},
			{label: "APA", value: "apa"},
			{label: "pubr", value: "pubr"},
			{label: qsTr("Detailed"), value: "detailed"}
		]
		info: qsTr("Choose the theme for prediction plots.")
	}
}
