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
	info: qsTr("Export model-derived values for each observation to the dataset. Exports follow the displayed model selection. When multiple models are displayed, column names include distribution, model, subgroup, and (for mixtures) component-count identifiers. Observations excluded from a model receive missing values.")

	TextField
	{
		name:			"exportColumnPrefix"
		label:			qsTr("Column prefix")
		defaultValue:	""
		fieldWidth:		160 * jaspTheme.uiScale
		Layout.columnSpan: 2
		info: qsTr("Optional custom prefix prepended to every exported column name, before any automatic model identifier.")
	}

	Group
	{
		title:	qsTr("Residuals")

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportResidualsMartingale"
			label:		qsTr("Martingale")
			info: qsTr("Export the event indicator minus the fitted cumulative hazard for each observation.")
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
			info: qsTr("Export observed time minus fitted mean survival time. Censored times are treated as observed times. Unavailable for interval-censored data.")
		}

		CheckBox
		{
			name:		"exportResidualsCoxSnell"
			label:		qsTr("Cox-Snell")
			enabled:	!exportRoot.intervalCensoring
			info: qsTr("Export the fitted cumulative hazard at each observation's time. For counting-process data, it is conditional on survival to the entry time. Unavailable for interval-censored data.")
		}
	}

	Group
	{
		title:	qsTr("Fitted Values")

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportFittedRisk"
			label:		qsTr("Relative risk")
			info: qsTr("Export the fitted relative risk, centered at the mean covariate values within each stratum.")
		}

		CheckBox
		{
			visible:	exportRoot.cox
			name:		"exportFittedLinearPredictor"
			label:		qsTr("Linear predictor")
			info: qsTr("Export the fitted log relative risk, centered at the mean covariate values within each stratum.")
		}

		CheckBox
		{
			visible:	!exportRoot.cox
			name:	"exportFittedMean"
			label:	qsTr("Mean survival time")
			info: qsTr("Export the fitted mean survival time at each observation's covariate values.")
		}

		CheckBox
		{
			visible:	!exportRoot.cox
			name:	"exportFittedMedian"
			label:	qsTr("Median survival time")
			info: qsTr("Export the fitted median survival time at each observation's covariate values.")
		}
	}

	Group
	{
		visible:	exportRoot.mixture
		title:	qsTr("Mixture Classification")

		CheckBox
		{
			name:	"exportMixtureProbabilities"
			label:	qsTr("Component probabilities")
			info: qsTr("Export one posterior membership probability per mixture component, conditional on the observed survival or censoring information.")
		}

		CheckBox
		{
			name:	"exportMixtureClassification"
			label:	qsTr("Component index")
			info: qsTr("Export the index of the component with the highest posterior membership probability. Ties are assigned to the first component.")
		}
	}
}
