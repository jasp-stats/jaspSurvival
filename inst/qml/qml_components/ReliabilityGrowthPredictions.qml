import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	property bool hasSubgroups: false
	title: qsTr("Predictions")

	Group
	{
		title: qsTr("Measures")
		CheckBox
		{
			name: "predictionsCumulativeFailures"
			label: qsTr("Cumulative failures")
			checked: true
			info: qsTr("Predict the expected cumulative number of failures since time zero.")
		}
		CheckBox
		{
			name: "predictionsFailureIntensity"
			label: qsTr("Failure intensity")
			info: qsTr("Predict the instantaneous failure rate per unit time.")
		}
		CheckBox
		{
			name: "predictionsCumulativeMtbf"
			label: qsTr("Cumulative MTBF")
			info: qsTr("Predict cumulative time divided by expected cumulative failures.")
		}
		CheckBox
		{
			name: "predictionsInstantaneousMtbf"
			label: qsTr("Instantaneous MTBF")
			info: qsTr("Predict the reciprocal of instantaneous failure intensity. This is a local rate-based measure, not the expected waiting time to the next failure.")
		}
	}

	Group
	{
		title: qsTr("Output")
		CheckBox
		{
			name: "predictionsOutputPlot"
			label: qsTr("Plot")
			checked: true
			info: qsTr("Plot each selected measure against time.")
		}
		CheckBox
		{
			name: "predictionsOutputTable"
			label: qsTr("Table")
			info: qsTr("Show predictions at the specified evaluation times.")
		}
		CheckBox
		{
			name: "predictionsConfidenceIntervals"
			label: qsTr("Confidence intervals")
			checked: true
			childrenOnSameRow: true
			info: qsTr("Show pointwise confidence intervals for fitted quantities. These are not prediction intervals for future counts. Rate-based intervals are unavailable for piecewise models.")
			CIField
			{
				name: "predictionsConfidenceIntervalsLevel"
				info: qsTr("Set the confidence level for predictions.")
			}
		}
	}

	Group
	{
		title: qsTr("Time")
		DropDown
		{
			id: timeGrid
			name: "predictionsTimeGrid"
			label: qsTr("Values")
			startValue: "intervals"
			values: [
				{label: qsTr("Equal intervals"), value: "intervals"},
				{label: qsTr("Sequence"), value: "sequence"},
				{label: qsTr("Custom"), value: "custom"}
			]
			info: qsTr("Choose evaluation times directly. No prediction variables are required.")
		}
		IntegerField
		{
			name: "predictionsTimeIntervals"
			label: qsTr("Intervals")
			defaultValue: 10
			min: 1
			max: 500
			visible: timeGrid.value === "intervals"
			info: qsTr("Set the number of equal intervals between From and To.")
		}
		TextField
		{
			name: "predictionsTimeFrom"
			label: qsTr("From")
			placeholderText: qsTr("First endpoint")
			fieldWidth: 130 * jaspTheme.uiScale
			visible: timeGrid.value !== "custom"
			info: qsTr("Enter a positive starting time. Leave empty to use the first observed endpoint in each subgroup.")
		}
		TextField
		{
			name: "predictionsTimeTo"
			label: qsTr("To")
			placeholderText: qsTr("Maximum follow-up")
			fieldWidth: 130 * jaspTheme.uiScale
			visible: timeGrid.value !== "custom"
			info: qsTr("Enter the last prediction time. Use a value beyond observed follow-up to forecast. Leave empty to use the maximum follow-up in each subgroup.")
		}
		TextField
		{
			name: "predictionsTimeStep"
			label: qsTr("Step")
			placeholderText: qsTr("Automatic")
			fieldWidth: 130 * jaspTheme.uiScale
			visible: timeGrid.value === "sequence"
			info: qsTr("Enter a positive time increment. Leave empty to divide the range into ten intervals.")
		}
		TextField
		{
			name: "predictionsTimeValues"
			label: qsTr("Times")
			defaultValue: "10, 25, 50, 100"
			fieldWidth: 180 * jaspTheme.uiScale
			visible: timeGrid.value === "custom"
			info: qsTr("Enter positive prediction times separated by commas, in the original time units.")
		}
	}

	Group
	{
		title: qsTr("Display")
		CheckBox
		{
			name: "predictionsObservedData"
			label: qsTr("Observed data")
			checked: true
			info: qsTr("Add observed cumulative failures or cumulative MTBF to the corresponding plots.")
		}
		CheckBox
		{
			name: "predictionsMergeSubgroups"
			label: qsTr("Merge subgroups")
			enabled: hasSubgroups
			checked: true
			info: qsTr("Overlay subgroup curves in the same plot. Otherwise display a separate plot for each subgroup.")
		}
		DropDown
		{
			name: "predictionsXAxisTransformation"
			label: qsTr("X-axis transformation")
			startValue: "none"
			values: [{label: qsTr("None"), value: "none"}, {label: qsTr("Log"), value: "log"}]
			info: qsTr("Choose a linear or logarithmic time axis.")
		}
		DropDown
		{
			name: "predictionsYAxisTransformation"
			label: qsTr("Y-axis transformation")
			startValue: "none"
			values: [{label: qsTr("None"), value: "none"}, {label: qsTr("Log"), value: "log"}]
			info: qsTr("Choose a linear or logarithmic axis for the predicted measure.")
		}
		DropDown
		{
			name: "plotLegend"
			label: qsTr("Legend")
			startValue: "right"
			values: [
				{label: qsTr("Right"), value: "right"},
				{label: qsTr("Bottom"), value: "bottom"},
				{label: qsTr("Top"), value: "top"},
				{label: qsTr("Left"), value: "left"},
				{label: qsTr("None"), value: "none"}
			]
			info: qsTr("Choose the legend position.")
		}
		DropDown
		{
			name: "plotTheme"
			label: qsTr("Theme")
			startValue: "jasp"
			values: [
				{label: "JASP", value: "jasp"},
				{label: qsTr("White background"), value: "whiteBackground"},
				{label: qsTr("Light"), value: "light"},
				{label: qsTr("Minimal"), value: "minimal"},
				{label: "APA", value: "apa"},
				{label: "pubr", value: "pubr"}
			]
			info: qsTr("Choose the theme for prediction and diagnostic plots.")
		}
		ColorPalette { info: qsTr("Choose colors for subgroup curves.") }
	}
}
