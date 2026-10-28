import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id: diagnosticsRoot
	property bool leastSquares: false
	property bool hasSubgroups: false
	readonly property bool quantileSelected: probabilityPlot.checked ||
		((residualVsTime.checked || residualVsPredicted.checked || residualHistogram.checked) && residualType.value === "quantile")
	title: qsTr("Diagnostics")

	Group
	{
		title: qsTr("Residual Plots")
		CheckBox
		{
			id: residualVsTime
			name: "residualPlotResidualVsTime"
			label: qsTr("Residuals vs. time")
			info: qsTr("Plot the selected residuals against cumulative interval endpoints.")
		}
		CheckBox
		{
			id: residualVsPredicted
			name: "residualPlotResidualVsPredicted"
			label: qsTr("Residuals vs. predicted")
			info: qsTr("Plot residuals against fitted values on the regression scale, or fitted interval event counts for count residuals.")
		}
		CheckBox
		{
			id: residualHistogram
			name: "residualPlotResidualHistogram"
			label: qsTr("Residuals histogram")
			info: qsTr("Display the distribution of the selected residuals.")
		}
		DropDown
		{
			id: residualType
			name: "residualPlotResidualType"
			label: qsTr("Type")
			startValue: diagnosticsRoot.leastSquares ? "regression" : "pearson"
			values: (diagnosticsRoot.leastSquares ? [{label: qsTr("Regression"), value: "regression"}] : []).concat([
				{label: qsTr("Response"), value: "response"},
				{label: qsTr("Pearson"), value: "pearson"},
				{label: qsTr("Deviance"), value: "deviance"},
				{label: qsTr("Randomized quantile"), value: "quantile"}
			])
			info: qsTr("Regression residuals use the package's log-scale least-squares regression. Response, Pearson, deviance and randomized quantile residuals use Poisson interval event counts with means obtained from the fitted cumulative curve. Count residuals assume an NHPP, including for Duane. Set a seed under Advanced to reproduce randomized quantile residuals.")
		}
	}

	RecurrentEventProbabilityPlot
	{
		id: probabilityPlot
		hasSubgroups: diagnosticsRoot.hasSubgroups
	}
}
