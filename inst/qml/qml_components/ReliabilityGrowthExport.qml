import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id: exportRoot
	property bool leastSquares: false
	readonly property bool quantileSelected: exportQuantile.checked
	title: qsTr("Export")
	columns: 2
	info: qsTr("Add residuals and fitted counts as dataset columns, preserving the original row order.")

	TextField
	{
		name: "exportColumnPrefix"
		label: qsTr("Column prefix")
		defaultValue: "Growth"
		fieldWidth: 160 * jaspTheme.uiScale
		Layout.columnSpan: 2
		info: qsTr("Set a non-empty prefix for exported column names.")
	}
	Group
	{
		title: qsTr("Residuals")
		CheckBox
		{
			name: "exportResidualsRegression"
			label: qsTr("Regression")
			enabled: exportRoot.leastSquares
			info: qsTr("Export the package's log-scale regression residuals. Available for least-squares fits.")
		}
		CheckBox
		{
			name: "exportResidualsResponse"
			label: qsTr("Response")
			info: qsTr("Export observed minus fitted interval event counts.")
		}
		CheckBox
		{
			name: "exportResidualsPearson"
			label: qsTr("Pearson")
			info: qsTr("Export interval count differences divided by the square root of the fitted interval mean.")
		}
		CheckBox
		{
			name: "exportResidualsDeviance"
			label: qsTr("Deviance")
			info: qsTr("Export signed Poisson deviance residuals for observation intervals.")
		}
		CheckBox
		{
			id: exportQuantile
			name: "exportResidualsQuantile"
			label: qsTr("Randomized quantile")
			info: qsTr("Export the same cached normal quantile residuals used in diagnostic and probability plots. Set a seed under Advanced to reproduce them.")
		}
	}
	Group
	{
		title: qsTr("Fitted Values")
		CheckBox
		{
			name: "exportFittedValuesCumulativeFailures"
			label: qsTr("Cumulative events")
			info: qsTr("Export fitted cumulative events at each interval endpoint.")
		}
		CheckBox
		{
			name: "exportFittedValuesIntervalFailures"
			label: qsTr("Interval events")
			info: qsTr("Export expected events within each observation interval, obtained by differencing the fitted cumulative mean.")
		}
	}
}
