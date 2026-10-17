pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

ComponentsList
{
	id: modifiers
	property var models
	property bool mixture: false
	property int componentCount: 1
	property bool allowEyring: true
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false
	depends: models
	source: [{values: models.factorsTitles}]
	optionKey: "model"
	addItemManually: false
	addBorder: false
	rowSpacing: 10 * jaspTheme.uiScale
	info: qsTr("Set coefficient restrictions, covariate transformations and factor contrasts for each model.")

	rowComponent: ComponentsList
	{
		property bool mixture: listView.mixture
		property int componentCount: listView.componentCount
		property bool allowEyring: listView.allowEyring
		property var covariateNames: listView.covariateNames
		property var factorNames: listView.factorNames
		property bool predictorSourcesReady: listView.predictorSourcesReady
		implicitWidth: listView.availableWidth
		name: "predictors"
		source: [{name: rowValue, use: "noInteraction"}]
		addItemManually: false
		addBorder: true
		itemRectangle.color: jaspTheme.controlBackgroundColor
		optionKey: "variable"
		preferredWidth: implicitWidth
		rowSpacing: 4 * jaspTheme.uiScale
		headerLabels: [{variableColumn: rowLabel}].concat(
			mixture ? [{componentColumn: qsTr("Component")}, {componentParameterRestrictions: qsTr("Parameter restriction")}] : [{parameterRestriction: qsTr("Parameter restriction")}],
			[{predictorSpecification: qsTr("Predictor specification")}])

		rowComponent: RowLayout
		{
			id: predictorRow
			property bool mixture: listView.mixture
			property int componentCount: listView.componentCount
			property bool allowEyring: listView.allowEyring
			readonly property bool factorPredictor: listView.factorNames.indexOf(rowValue) >= 0
			readonly property bool covariatePredictor: listView.covariateNames.indexOf(rowValue) >= 0
			spacing: 8 * jaspTheme.uiScale
			Group
			{
				name: "variableColumn"
				Layout.preferredWidth: 150 * jaspTheme.uiScale
				Layout.alignment: predictorRow.mixture ? Qt.AlignTop : Qt.AlignVCenter
				Label
				{
					text: rowLabel
					height: predictorRow.mixture ? componentRestrictions.rowHeight : implicitHeight
					verticalAlignment: Text.AlignVCenter
				}
			}
			Group
			{
				name: "componentColumn"
				visible: predictorRow.mixture
				Layout.preferredWidth: visible ? 60 * jaspTheme.uiScale : 0
				Layout.alignment: predictorRow.mixture ? Qt.AlignTop : Qt.AlignVCenter
				Column
				{
					spacing: componentRestrictions.rowSpacing
					Repeater
					{
						model: predictorRow.componentCount
						Label
						{
							text: (index + 1).toString()
							height: componentRestrictions.rowHeight
							verticalAlignment: Text.AlignVCenter
						}
					}
				}
			}
			ParametricFixedParameter
			{
				name: "parameterRestriction"
				visible: !predictorRow.mixture
				positive: false
				fieldWidth: 125 * jaspTheme.uiScale
				info: qsTr("Fix regression coefficients at specified values or expressions. Leave empty to estimate them; separate multiple values with commas or semicolons.")
			}
			ParametricRegressionComponentRestrictions
			{
				id: componentRestrictions
				name: "componentParameterRestrictions"
				componentCount: predictorRow.componentCount
				visible: predictorRow.mixture
			}
			Group
			{
				name: "predictorSpecification"
				ParametricTermTransformation
				{
					id: transformation
					visible: predictorRow.covariatePredictor
					enabled: visible
					allowScale: predictorRow.covariatePredictor
					allowSpline: predictorRow.covariatePredictor
					allowEyring: predictorRow.allowEyring
				}
				DropDown
				{
					id: factorContrast
					name: "factorContrast"
					visible: predictorRow.factorPredictor
					enabled: visible
					startValue: "treatment"
					fieldWidth: 150 * jaspTheme.uiScale
					useExternalBorder: true
					values: [
						{label: qsTr("Treatment (dummy)"), value: "treatment"},
						{label: qsTr("Sum"), value: "sum"},
						{label: qsTr("Helmert"), value: "helmert"},
						{label: qsTr("Orthogonal polynomial"), value: "poly"},
						{label: qsTr("SAS"), value: "SAS"}
					]
					info: qsTr("Choose how to code the factor's levels in the regression model.")
				}
			}
			DropDown
			{
				name: "factorReference"
				label: qsTr("Reference")
				visible: predictorRow.factorPredictor && (factorContrast.value === "treatment" || factorContrast.value === "SAS")
				enabled: visible
				addEmptyValue: false
				source: [{values: [rowValue], use: "levels"}]
				fieldWidth: 150 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Select the factor level used as the reference for Treatment or SAS coding.")
			}
			FormulaField
			{
				name: "predictorTransformationBase"
				label: qsTr("Base")
				visible: predictorRow.covariatePredictor && (transformation.value === "customPower")
				enabled: predictorRow.covariatePredictor && (transformation.value === "customPower")
				defaultValue: "10"
				min: 0
				inclusive: JASP.MaxOnly
				fieldWidth: 40 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Set the base of the logarithm. Use a positive value other than 1.")
			}
			FormulaField
			{
				name: "predictorTransformationMultiplier"
				label: qsTr("Multiplier")
				visible: predictorRow.covariatePredictor && (transformation.value === "customPower")
				enabled: predictorRow.covariatePredictor && (transformation.value === "customPower")
				defaultValue: "1"
				fieldWidth: 40 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Set a nonzero multiplier for the logarithm.")
			}
			FormulaField
			{
				name: "predictorTransformationExponent"
				label: qsTr("Exponent")
				visible: predictorRow.covariatePredictor && (transformation.value === "customExponent")
				enabled: predictorRow.covariatePredictor && (transformation.value === "customExponent")
				defaultValue: "2"
				fieldWidth: 40 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Set the exponent for the power transformation.")
			}
			IntegerField
			{
				name: "predictorTransformationDegreesOfFreedom"
				label: qsTr("Degrees of freedom")
				visible: predictorRow.covariatePredictor && (transformation.value === "naturalSpline")
				enabled: predictorRow.covariatePredictor && (transformation.value === "naturalSpline")
				defaultValue: 3
				min: 1
				fieldWidth: 40 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Set the degrees of freedom for the natural cubic spline. Higher values allow more flexible curves.")
			}
			FormulaField
			{
				name: "predictorTransformationRootDegree"
				label: qsTr("Root degree")
				visible: predictorRow.covariatePredictor && (transformation.value === "root")
				enabled: predictorRow.covariatePredictor && (transformation.value === "root")
				defaultValue: "2"
				fieldWidth: 40 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Set the degree of the root. A value of 2 gives a square root.")
			}
			IntegerField
			{
				name: "predictorTransformationDegree"
				label: qsTr("Degree")
				visible: predictorRow.covariatePredictor && (transformation.value === "polynomial")
				enabled: predictorRow.covariatePredictor && (transformation.value === "polynomial")
				defaultValue: 2
				min: 1
				fieldWidth: 40 * jaspTheme.uiScale
				useExternalBorder: true
				info: qsTr("Set the polynomial degree: 1 for linear, 2 for quadratic, 3 for cubic.")
			}
			DropDown
			{
				name: "predictorTransformationFrom"
				label: qsTr("From")
				visible: predictorRow.covariatePredictor && (transformation.value === "arrhenius" || transformation.value === "eyring")
				startValue: "celsius"
				fieldWidth: 100 * jaspTheme.uiScale
				useExternalBorder: true
				values: [
					{label: qsTr("Kelvin"), value: "kelvin"},
					{label: qsTr("Celsius"), value: "celsius"},
					{label: qsTr("Fahrenheit"), value: "fahrenheit"}
				]
				info: qsTr("Select the units of the temperature variable.")
			}
		}
	}
}
