import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id: predictionsRoot
	property bool hasSubgroups: false
	readonly property bool outcomeSelected: outcomeCount > 0
	readonly property int outcomeCount: Number(survivalProbability.checked) + Number(failureProbability.checked) + Number(cumulativeEvents.checked) + Number(eventIntensity.checked) + Number(cumulativeMeanTimeBetweenEvents.checked) + Number(instantaneousMeanTimeBetweenEvents.checked)
	readonly property bool predictionOutputSelected: outcomeSelected && (modelPlot.checked || modelTable.checked)
	title: qsTr("Predictions")

	Group
	{
		title: qsTr("Outcome")
		CheckBox
		{
			id: survivalProbability
			name: "predictionsOutcomeSurvivalProbability"
			label: qsTr("Survival probability")
			checked: false
			info: qsTr("Predict event-free survival: the probability of no events from time zero to each selected time, exp(-M(t)), assuming an NHPP. For Duane, this assumes an NHPP interpretation of its fitted growth curve. This does not add patient-level censoring support.")
		}
		CheckBox
		{
			id: failureProbability
			name: "predictionsOutcomeFailureProbability"
			label: qsTr("Failure probability")
			checked: false
			info: qsTr("Predict the probability of at least one event from time zero to each selected time, 1 - exp(-M(t)), assuming an NHPP. For Duane, this assumes an NHPP interpretation of its fitted growth curve. This is the complement of event-free survival.")
		}
		CheckBox
		{
			id: cumulativeEvents
			name: "predictionsOutcomeCumulativeEvents"
			label: qsTr("Cumulative events")
			checked: false
			info: qsTr("Predict the expected cumulative number of events since time zero.")
		}
		CheckBox
		{
			id: eventIntensity
			name: "predictionsOutcomeEventIntensity"
			label: qsTr("Event intensity")
			info: qsTr("Predict the instantaneous event rate per unit time.")
		}
		CheckBox
		{
			id: cumulativeMeanTimeBetweenEvents
			name: "predictionsOutcomeCumulativeMeanTimeBetweenEvents"
			label: qsTr("Cumulative mean time between events")
			info: qsTr("Predict cumulative time divided by expected cumulative events. This is an average time per event up to the selected time.")
		}
		CheckBox
		{
			id: instantaneousMeanTimeBetweenEvents
			name: "predictionsOutcomeInstantaneousMeanTimeBetweenEvents"
			label: qsTr("Instantaneous mean time between events")
			info: qsTr("Predict the reciprocal of instantaneous event intensity. This is a local rate-based measure, not the expected waiting time to the next event when the rate changes.")
		}
	}

	Group
	{
		PredictionTime
		{
			id: predictionTime
			enabled: predictionsRoot.predictionOutputSelected
			fromPlaceholder: qsTr("First endpoint")
			intervalsInfo: qsTr("Set the number of equal intervals between the first observed endpoint and maximum follow-up in each subgroup.")
			fromInfo: qsTr("Set a positive first time in the sequence. Leave blank to use the first observed endpoint in each subgroup.")
			toInfo: qsTr("Set the last prediction time. Use a time beyond observed follow-up to forecast. Leave blank to use the maximum follow-up in each subgroup.")
			valuesInfo: qsTr("Enter positive prediction times separated by commas, in the original time units. Leave blank to use 25%, 50% and 75% of follow-up.")
		}
		Group
		{
			title: qsTr("Output")
			enabled: predictionsRoot.outcomeSelected
			CheckBox
			{
				id: modelPlot
				name: "predictionsOutputPlot"
				label: qsTr("Plot")
				checked: true
				info: qsTr("Plot every selected measure in its own panel.")
			}
			CheckBox
			{
				id: modelTable
				name: "predictionsOutputTable"
				label: qsTr("Table")
				checked: false
				childrenOnSameRow: true
				info: qsTr("Show a table of the selected predictions.")
				DropDown
				{
					name: "predictionsOutputTableFormat"
					label: ""
					startValue: "wide"
					values: [{label: qsTr("Long"), value: "long"}, {label: qsTr("Wide"), value: "wide"}]
					info: qsTr("Show times in rows (Long) or separate column groups (Wide).")
				}
			}
		}
	}

	Group
	{
		enabled: modelPlot.checked && predictionsRoot.outcomeSelected
		DropDown
		{
			name: "predictionsYAxisScale"
			label: qsTr("Y-axis scale")
			startValue: "none"
			values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}]
			info: qsTr("Choose the y-axis scale for the predicted quantities.")
		}
		DropDown
		{
			name: "predictionsXAxisTransformation"
			label: qsTr("X-axis transformation")
			startValue: "none"
			values: [{label: qsTr("Linear"), value: "none"}, {label: qsTr("Log"), value: "log"}]
			info: qsTr("Choose the x-axis scale for time.")
		}
	}

	Group
	{
		title: qsTr("Display")
		CheckBox
		{
			name: "predictionsDisplayConfidenceIntervals"
			label: qsTr("Confidence intervals")
			checked: true
			enabled: predictionsRoot.predictionOutputSelected
			childrenOnSameRow: true
			info: qsTr("Include pointwise confidence intervals for the figures and tables. These are not prediction intervals for future counts. Rate-based intervals are unavailable for piecewise models.")
			CIField
			{
				name: "predictionsDisplayConfidenceIntervalsLevel"
				info: qsTr("Set the confidence level for the confidence intervals.")
			}
		}
		PredictionPlotAppearance
		{
			legendAvailable: modelPlot.checked && predictionsRoot.outcomeSelected
			plotsAvailable: modelPlot.checked && predictionsRoot.outcomeSelected
		}
		CheckBox
		{
			name: "predictionsDisplayObservedData"
			label: qsTr("Observed data")
			checked: true
			enabled: modelPlot.checked && (cumulativeEvents.checked || cumulativeMeanTimeBetweenEvents.checked)
			info: qsTr("Add observed cumulative events or cumulative mean time between events to the corresponding plots.")
		}
		ColorPalette
		{
			enabled: modelPlot.checked && predictionsRoot.outcomeSelected
			info: qsTr("Colors for prediction series.")
		}
	}

	Group
	{
		columns: 1
		Group
		{
			title: qsTr("Merge series across")
			columns: 2
			CheckBox
			{
				name: "predictionsMergeSeriesAcrossSubgroups"
				label: qsTr("Subgroups")
				checked: false
				enabled: predictionsRoot.hasSubgroups && modelPlot.checked && predictionsRoot.outcomeSelected
				info: qsTr("Overlay predictions for the separately fitted subgroups.")
			}
		}
		CheckBox
		{
			name: "predictionsMergeTablesAcrossMeasures"
			label: qsTr("Merge tables across measures")
			checked: false
			enabled: modelTable.checked && predictionsRoot.outcomeCount > 1
			info: qsTr("Combine the selected time-based prediction measures in one table.")
		}
		CheckBox
		{
			name: "predictionsRoundTimeTableValues"
			label: qsTr("Round time table values")
			checked: false
			visible: predictionTime.gridControl.value === "quantiles" || predictionTime.gridControl.value === "sequence"
			enabled: modelTable.checked && predictionsRoot.outcomeSelected
			info: qsTr("Round evaluation times in tables to the nearest integer. Rounded times must remain positive.")
		}
	}
}
