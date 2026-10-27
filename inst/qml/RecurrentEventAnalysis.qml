import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP
import "./qml_components" as SA

Form
{
	id: form
	info: qsTr("Model recurrent events from event counts in successive observation intervals. Fit each independent test sequence separately using Subgroup.")

	VariablesForm
	{
		preferredHeight: 280 * jaspTheme.uiScale

		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList
		{
			name: "time"
			title: qsTr("Time")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Select cumulative interval endpoints or interval durations. Rows must follow the observation order within each subgroup.")
		}
		AssignedVariablesList
		{
			name: "eventCount"
			title: qsTr("Event Count (optional)")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Select the number of events within each interval, not cumulative counts. Leave empty for one event per interval. ReliaGrowR currently requires positive counts in every interval.")
		}
		AssignedVariablesList
		{
			id: subgroup
			name: "subgroup"
			title: qsTr("Subgroup")
			allowedColumns: ["nominal"]
			info: qsTr("Fit each combination of factor levels as an independent observation sequence. Do not split one sequence by changing operating conditions.")
		}
	}

	Group
	{
		DropDown
		{
			id: model
			name: "model"
			label: qsTr("Model")
			startValue: "crowAmsaa"
			values: [
				{label: qsTr("Duane"), value: "duane"},
				{label: qsTr("Crow–AMSAA (power-law NHPP)"), value: "crowAmsaa"},
				{label: qsTr("Log-linear NHPP"), value: "logLinear"},
				{label: qsTr("Piecewise power-law NHPP"), value: "piecewise"}
			]
			info: qsTr("Choose the recurrent-event model. Crow–AMSAA is a power-law non-homogeneous Poisson process (NHPP).")
		}
		DropDown
		{
			id: estimationMethod
			name: "estimationMethod"
			label: qsTr("Estimation")
			depends: model
			enabled: model.value === "crowAmsaa"
			startValue: model.value === "duane" || model.value === "piecewise" ? "ls" : "mle"
			values: model.value === "crowAmsaa" ? [
				{label: qsTr("Maximum likelihood"), value: "mle"},
				{label: qsTr("Least squares"), value: "ls"}
			] : model.value === "logLinear" ? [
				{label: qsTr("Maximum likelihood"), value: "mle"}
			] : [{label: qsTr("Least squares"), value: "ls"}]
			info: qsTr("Maximum likelihood uses counts in observation intervals. Least squares fits a regression on logarithms of cumulative quantities.")
		}
		DropDown
		{
			name: "timeType"
			label: qsTr("Time input")
			startValue: "cumulative"
			values: [
				{label: qsTr("Cumulative interval endpoints"), value: "cumulative"},
				{label: qsTr("Interval durations"), value: "interval"}
			]
			info: qsTr("Choose how Time describes the observation intervals. Each sequence starts at zero. Maximum likelihood uses an interval-count likelihood rather than an exact-event-time likelihood.")
		}
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			CheckBox
			{
				name: "modelSummary"
				label: qsTr("Model summary")
				checked: true
				info: qsTr("Include a table with information about the model fit.")
			}
			CheckBox
			{
				name: "dataSummary"
				label: qsTr("Data summary")
				checked: false
				info: qsTr("Show observation intervals, total events, follow-up time and observed cumulative mean time between events for each subgroup.")
			}
		}

		Group
		{
			CheckBox
			{
				name: "coefficients"
				label: qsTr("Coefficients")
				checked: true
				info: qsTr("Include a table with coefficient estimates.")
				CheckBox
				{
					name: "coefficientsConfidenceIntervals"
					label: qsTr("Confidence intervals")
					checked: true
					childrenOnSameRow: true
					info: qsTr("Include confidence intervals for the coefficients.")
					CIField
					{
						name: "coefficientsConfidenceIntervalsLevel"
						info: qsTr("Set the confidence level for the confidence intervals.")
					}
				}
			}
			CheckBox
			{
				name: "coefficientsCovarianceMatrix"
				label: qsTr("Coefficients covariance matrix")
				checked: false
				info: qsTr("Include a table with the covariance matrix of the coefficient estimates.")
			}
			Group
			{
				visible: model.value === "piecewise"
				CheckBox
				{
					name: "piecewiseSegmentEstimates"
					label: qsTr("Segment estimates")
					checked: true
					info: qsTr("Show segment slopes, their standard errors, growth rates and scales returned by ReliaGrowR.")
				}
				CheckBox
				{
					name: "piecewiseChangePoints"
					label: qsTr("Change points")
					checked: true
					info: qsTr("Show estimated change-point times in the original time units and native standard errors on the log-time scale.")
				}
			}
		}

		CheckBox
		{
			name: "fittedModelEquation"
			label: qsTr("Fitted model equation")
			checked: false
			info: qsTr("Show the fitted model equations in a table.")
			IntegerField
			{
				name: "fittedModelEquationDecimals"
				label: qsTr("Decimals")
				defaultValue: 3
				min: 0
				max: 15
				info: qsTr("Set the number of decimal places displayed in the fitted equations.")
			}
		}
	}

	SA.RecurrentEventPredictions { hasSubgroups: subgroup.count > 0 }

	SA.RecurrentEventDiagnostics
	{
		id: diagnostics
		leastSquares: estimationMethod.value === "ls"
		hasSubgroups: subgroup.count > 0
	}

	SA.RecurrentEventExport
	{
		id: eventExport
		leastSquares: estimationMethod.value === "ls"
	}

	Section
	{
		title: qsTr("Advanced")
		columns: 2
		Group
		{
			columns: 1
			Group
			{
				title: qsTr("Confidence interval")
				DropDown
				{
					name: "confidenceIntervalBounds"
					label: qsTr("Bounds")
					startValue: "twoSided"
					values: [
						{label: qsTr("Two-sided"), value: "twoSided"},
						{label: qsTr("Lower"), value: "lower"},
						{label: qsTr("Upper"), value: "upper"}
					]
					info: qsTr("Choose two-sided confidence intervals or one-sided lower or upper bounds.")
				}
			}
			Group
			{
				title: qsTr("Model Options")
				visible: model.value === "piecewise"
				CheckBox
				{
					name: "piecewiseStartingBreakpoints"
					label: qsTr("Starting change points")
					info: qsTr("Provide initial locations for estimated change points. When unchecked, the package initializes one change point automatically. These locations are starting values, not fixed change points.")
					TextField
					{
						name: "piecewiseStartingBreakpointsValues"
						label: qsTr("Times")
						fieldWidth: 180 * jaspTheme.uiScale
						info: qsTr("Enter starting change-point times in the original time units, separated by commas.")
					}
				}
			}
		}
		SetSeed
		{
			title: qsTr("Reproducibility")
			enabled: model.value === "piecewise" || diagnostics.quantileSelected || eventExport.quantileSelected
		}
	}
}
