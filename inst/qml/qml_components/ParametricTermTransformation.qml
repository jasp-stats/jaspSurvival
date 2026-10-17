pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Controls as QtControls
import QtQuick.Layouts
import JASP.Controls
import JASP

// Rich labels keep mathematical indices readable in both the selection and menu.
DropDown
{
	id: transformation
	property bool allowScale: false
	property bool allowSpline: false
	property bool allowEyring: true
	Layout.alignment: Qt.AlignTop
	name: "predictorTransformation"
	startValue: "none"
	fieldWidth: 150 * jaspTheme.uiScale
	useExternalBorder: true
	control.contentItem: Text
	{
		text: transformation.currentLabel
		textFormat: Text.RichText
		font: transformation.control.font
		color: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		verticalAlignment: Text.AlignVCenter
		leftPadding: 4 * jaspTheme.uiScale
		clip: true
	}
	control.delegate: QtControls.ItemDelegate
	{
		id: transformationItem
		required property int index
		required property var model
		width: transformation ? transformation.control.popup.width : 0
		height: jaspTheme.comboBoxHeight
		readonly property bool selected: transformation && transformation.currentIndex === index
		highlighted: transformation && transformation.control.highlightedIndex === index
		contentItem: Text
		{
			text: model.name
			textFormat: Text.RichText
			font: jaspTheme.font
			color: transformationItem.selected ? jaspTheme.white : jaspTheme.textEnabled
			verticalAlignment: Text.AlignVCenter
		}
		background: Rectangle
		{
			color: transformationItem.selected ? jaspTheme.itemSelectedColor :
				(transformationItem.hovered || transformationItem.highlighted ? jaspTheme.itemHoverColor : "transparent")
		}
	}
	values:
	{
		let transformations = [
			{label: qsTr("None"), value: "none"},
			// Retain the stored identifiers for existing log and exponent operations.
			{label: qsTr("Natural log: ln(x)"), value: "power"},
			{label: qsTr("General log (c, y): c log<sub>y</sub>(x)"), value: "customPower"},
			{label: qsTr("Arrhenius (eV): 1/(k<sub>B</sub>T)"), value: "arrhenius"},
			{label: qsTr("Reciprocal: 1/x"), value: "reciprocal"},
			{label: qsTr("Power (y): x<sup>y</sup>"), value: "customExponent"},
			{label: qsTr("Root (y): <sup>y</sup>√x"), value: "root"},
			{label: qsTr("Polynomial"), value: "polynomial"}
		];
		if (allowScale)
			transformations.splice(1, 0, {label: qsTr("Scale: (x - mean(x)) / sd(x)"), value: "scale"});
		if (allowSpline)
			transformations.push({label: qsTr("Natural cubic spline"), value: "naturalSpline"});
		if (allowEyring)
			transformations.splice(transformations.findIndex(function(item) { return item.value === "arrhenius"; }) + 1, 0,
				{label: qsTr("Eyring: 1/T and ln(T)"), value: "eyring"});
		return transformations;
	}
	info: qsTr("Choose a transformation for the covariate before fitting the model. Arrhenius and Eyring are for temperature effects.")
}
