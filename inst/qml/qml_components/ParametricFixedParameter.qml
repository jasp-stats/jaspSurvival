pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import JASP.Controls
import JASP

FormulaField
{
	property bool positive: true
	defaultValue: ""
	// Preserve empty expressions: FormulaField's numeric binding turns "" into zero.
	// The fitter evaluates and validates these optional expressions on the native scale.
	inputType: "string"
	parseDefaultValue: false
	min: positive ? 0 : -Infinity
	inclusive: positive ? JASP.MaxOnly : JASP.MinMax
	fieldWidth: 55 * jaspTheme.uiScale
	useExternalBorder: true
	info: qsTr("Specify a fixed value or a numeric expression. Leave empty to estimate this parameter.")
}
