pragma Translator: "ParametricMixtureSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id: exportRoot
	property bool cox: false
	property bool mixture: false
	property bool intervalCensoring: false

	title:		qsTr("Export")
	columns:	2
	info: qsTr("Add residuals, fitted values or mixture classifications from the displayed models as new columns in the dataset.")

	TextField
	{
		name:			"exportColumnPrefix"
		label:			qsTr("Column prefix")
		defaultValue:	""
		fieldWidth:		160 * jaspTheme.uiScale
		Layout.columnSpan: 2
		info: qsTr("Set a prefix for the names of exported columns.")
	}

	Group
	{
		title:	qsTr("Residuals")

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportResidualsMartingale"
			label:		qsTr("Martingale")
			info: qsTr("Export martingale residuals for each observation.")
		}

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportResidualsDeviance"
			label:		qsTr("Deviance")
			info: qsTr("Export deviance residuals from the fitted Cox model for each observation.")
		}

		CheckBox
		{
			visible:	!exportRoot.cox
			name:		"exportResidualsResponse"
			label:		qsTr("Response")
			enabled:	!exportRoot.intervalCensoring
			info: qsTr("Export observed minus predicted mean survival time, treating censored times as observed. Unavailable for interval-censored data.")
		}

		CheckBox
		{
			name:		"exportResidualsCoxSnell"
			label:		qsTr("Cox-Snell")
			enabled:	!exportRoot.intervalCensoring
			info: qsTr("Export Cox-Snell residuals. For counting-process data, these are conditional on survival to entry. Unavailable for interval-censored data.")
		}
	}

	Group
	{
		title:	qsTr("Fitted Values")

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportFittedValuesRelativeRisk"
			label:		qsTr("Relative risk")
			info: qsTr("Export the fitted relative risk, centered at mean covariate values within each stratum.")
		}

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportFittedValuesLinearPredictor"
			label:		qsTr("Linear predictor")
			info: qsTr("Export the fitted log relative risk, centered at mean covariate values within each stratum.")
		}

		CheckBox
		{
			visible:	!exportRoot.cox
			name:	"exportFittedValuesMeanSurvivalTime"
			label:	qsTr("Mean survival time")
			info: qsTr("Export the fitted mean survival time for each observation.")
		}

		CheckBox
		{
			visible:	!exportRoot.cox
			name:	"exportFittedValuesMedianSurvivalTime"
			label:	qsTr("Median survival time")
			info: qsTr("Export the fitted median survival time for each observation.")
		}
	}

	Group
	{
		visible:	exportRoot.mixture
		title:	qsTr("Mixture Classification")

		CheckBox
		{
			name:	"exportMixtureClassificationComponentProbabilities"
			label:	qsTr("Component probabilities")
			info: qsTr("Export the estimated probability that each observation belongs to each mixture component.")
		}

		CheckBox
		{
			name:	"exportMixtureClassificationComponentIndex"
			label:	qsTr("Component index")
			info: qsTr("Export the most likely mixture component for each observation.")
		}
	}
}
