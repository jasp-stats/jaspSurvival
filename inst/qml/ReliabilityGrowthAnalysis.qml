import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP
import "./qml_components" as SA

Form
{
	id: form
	info: qsTr("Model reliability growth from failure counts in successive observation intervals. Fit each independent test sequence separately using Subgroup.")

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
			name: "failures"
			title: qsTr("Failure Count (optional)")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Select the number of failures within each interval, not cumulative counts. Leave empty for one failure per interval. ReliaGrowR currently requires positive counts in every interval.")
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
			info: qsTr("Choose the reliability-growth model. Crow–AMSAA is a power-law non-homogeneous Poisson process (NHPP).")
		}
		DropDown
		{
			id: estimationMethod
			name: "estimationMethod"
			label: qsTr("Estimation")
			depends: model
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
		CheckBox
		{
			name: "dataSummary"
			label: qsTr("Data summary")
			checked: true
			info: qsTr("Show the number of observation intervals, total failures, follow-up time and observed cumulative MTBF.")
		}
		CheckBox
		{
			name: "modelSummary"
			label: qsTr("Model summary")
			checked: true
			info: qsTr("Show the estimation method, log-likelihood, parameter count, AIC and BIC.")
		}
		CheckBox
		{
			name: "coefficients"
			label: qsTr("Coefficients")
			checked: true
			info: qsTr("Show the model parameters with standard errors.")
			CheckBox
			{
				name: "coefficientsConfidenceIntervals"
				label: qsTr("Confidence intervals")
				checked: true
				childrenOnSameRow: true
				info: qsTr("Include confidence intervals for the parameters.")
				CIField
				{
					name: "coefficientsConfidenceIntervalsLevel"
					info: qsTr("Set the confidence level for the parameters.")
				}
			}
		}
		CheckBox
		{
			name: "coefficientsCovarianceMatrix"
			label: qsTr("Coefficients covariance matrix")
			info: qsTr("Show the covariance matrix on the reported parameter scale.")
		}
		CheckBox
		{
			name: "fittedModelEquation"
			label: qsTr("Fitted model equation")
			info: qsTr("Show the fitted cumulative mean or cumulative MTBF equation.")
			IntegerField
			{
				name: "fittedModelEquationDecimals"
				label: qsTr("Decimals")
				defaultValue: 3
				min: 0
				max: 15
				info: qsTr("Set the number of decimal places in the fitted equations.")
			}
		}
	}

	SA.ReliabilityGrowthPredictions { hasSubgroups: subgroup.count > 0 }

	SA.ReliabilityGrowthDiagnostics
	{
		id: diagnostics
		leastSquares: estimationMethod.value === "ls"
		hasSubgroups: subgroup.count > 0
	}

	SA.ReliabilityGrowthExport
	{
		id: growthExport
		leastSquares: estimationMethod.value === "ls"
	}

	Section
	{
		title: qsTr("Advanced")
		CheckBox
		{
			name: "piecewiseStartingBreakpoints"
			label: qsTr("Starting change points")
			enabled: model.value === "piecewise"
			info: qsTr("Provide initial locations for estimated change points. When unchecked, the package initializes one change point automatically. These locations are starting values, not fixed change points.")
			TextField
			{
				name: "piecewiseStartingBreakpointsValues"
				label: qsTr("Times")
				fieldWidth: 180 * jaspTheme.uiScale
				info: qsTr("Enter starting change-point times in the original time units, separated by commas.")
			}
		}
		SetSeed { enabled: model.value === "piecewise" || diagnostics.quantileSelected || growthExport.quantileSelected }
	}
}
