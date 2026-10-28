import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id: predictionsRoot
	property bool hasSubgroups: false
	readonly property bool outcomeSelected: outcomeCount > 0
	readonly property int outcomeCount: Number(missionReliability.checked) + Number(survivalProbability.checked) + Number(failureProbability.checked) + Number(cumulativeEvents.checked) + Number(eventIntensity.checked) + Number(cumulativeMeanTimeBetweenEvents.checked) + Number(instantaneousMeanTimeBetweenEvents.checked)
	readonly property bool predictionOutputSelected: outcomeSelected && (modelPlot.checked || modelTable.checked)
	title: qsTr("Predictions")

	Group
	{
		title: qsTr("Outcome")
		CheckBox
		{
			id: missionReliability
			name: "predictionsOutcomeMissionReliability"
			label: qsTr("Mission reliability")
			info: qsTr("Predict the probability of no events during a mission starting at each selected time: exp(-(M(t + d) - M(t))), where d is the mission duration. This assumes an NHPP, including for the Duane growth curve.")
		}
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
		Group
		{
			title: qsTr("Time")
			enabled: predictionsRoot.predictionOutputSelected
			DropDown
			{
				id: timeGrid
				name: "predictionsTimeGrid"
				label: qsTr("Grid")
				startValue: "quantiles"
				values: [
					{label: qsTr("Equal intervals"), value: "quantiles"},
					{label: qsTr("Sequence"), value: "sequence"},
					{label: qsTr("Custom (relative)"), value: "customRelative"},
					{label: qsTr("Custom (absolute)"), value: "customAbsolute"}
				]
				info: qsTr("Choose how to specify prediction times: equal intervals, a sequence or custom values.")
			}
			IntegerField
			{
				name: "predictionsTimeIntervals"
				label: qsTr("Intervals")
				defaultValue: 10
				min: 1
				max: 10000
				visible: timeGrid.value === "quantiles"
				info: qsTr("Set the number of equal intervals between the first observed endpoint and maximum follow-up in each subgroup.")
			}
			Group
			{
				columns: 3
				alignFields: false
				visible: timeGrid.value === "sequence"
				FormulaField
				{
					name: "predictionsTimeFrom"
					label: qsTr("From")
					defaultValue: ""
					inputType: "string"
					parseDefaultValue: false
					placeholderText: qsTr("First endpoint")
					fieldWidth: 50 * jaspTheme.uiScale
					info: qsTr("Set a positive first time in the sequence. Leave blank to use the first observed endpoint in each subgroup.")
				}
				FormulaField
				{
					name: "predictionsTimeStep"
					label: qsTr("Step")
					defaultValue: ""
					inputType: "string"
					parseDefaultValue: false
					placeholderText: qsTr("(To - From) / 10")
					fieldWidth: 50 * jaspTheme.uiScale
					info: qsTr("Set a positive time increment. Leave blank to use (To - From) / 10, or From when both endpoints are equal.")
				}
				FormulaField
				{
					name: "predictionsTimeTo"
					label: qsTr("To")
					defaultValue: ""
					inputType: "string"
					parseDefaultValue: false
					placeholderText: qsTr("Maximum follow-up")
					fieldWidth: 50 * jaspTheme.uiScale
					info: qsTr("Set the last prediction time. Use a time beyond observed follow-up to forecast. Leave blank to use the maximum follow-up in each subgroup.")
				}
			}
			FormulaField
			{
				name: "predictionsTimePercentages"
				label: qsTr("Percentages")
				defaultValue: "25, 50, 75"
				inputType: "string"
				parseDefaultValue: false
				fieldWidth: 150 * jaspTheme.uiScale
				visible: timeGrid.value === "customRelative"
				info: qsTr("Enter prediction times as percentages of the maximum follow-up time, separated by commas.")
			}
			FormulaField
			{
				name: "predictionsTimeValues"
				label: qsTr("Values")
				inputType: "string"
				parseDefaultValue: false
				placeholderText: qsTr("25%, 50%, 75% of follow-up")
				fieldWidth: 180 * jaspTheme.uiScale
				visible: timeGrid.value === "customAbsolute"
				info: qsTr("Enter positive prediction times separated by commas, in the original time units. Leave blank to use 25%, 50% and 75% of follow-up.")
			}
			DoubleField
			{
				name: "predictionsMissionDuration"
				label: qsTr("Mission duration")
				defaultValue: 1
				min: 0
				inclusive: JASP.MaxOnly
				enabled: missionReliability.checked
				info: qsTr("Set a positive mission duration in the original time units. The time grid specifies mission start times.")
			}
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
		DropDown
		{
			name: "plotLegend"
			label: qsTr("Legend")
			startValue: "right"
			enabled: modelPlot.checked && predictionsRoot.outcomeSelected
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
			enabled: modelPlot.checked && predictionsRoot.outcomeSelected
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
			visible: timeGrid.value === "quantiles" || timeGrid.value === "sequence"
			enabled: modelTable.checked && predictionsRoot.outcomeSelected
			info: qsTr("Round evaluation times in tables to the nearest integer. Rounded times must remain positive.")
		}
	}
}
