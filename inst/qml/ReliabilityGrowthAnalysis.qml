import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP
import "./qml_components" as SA

Form
{
	id: form
	info: qsTr("Model reliability growth from event counts in successive observation intervals. Fit each independent test sequence separately using Subgroup.")

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
			info: qsTr("Choose the reliability-growth model. Crow–AMSAA is a power-law non-homogeneous Poisson process (NHPP).")
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

	SA.ParametricStatistics
	{
		modelComparisonVisible: false
		modelSummaryInfo: qsTr("Show the reliability-growth model, estimation method, log-likelihood, parameter count, AIC and BIC for each independently fitted subgroup.")
		coefficientsInfo: qsTr("Show reliability-growth parameter estimates and standard errors. Confidence intervals follow the model's estimation method.")
		covarianceInfo: qsTr("Show the covariance matrix on the reported parameter scale, with one matrix per fitted subgroup.")
		equationInfo: qsTr("Show fitted equations for cumulative events, event intensity or cumulative mean time between events. Small nonzero coefficients are displayed in scientific notation when needed.")
		extraSummaryControls: [
			CheckBox
			{
				name: "dataSummary"
				label: qsTr("Data summary")
				checked: true
				info: qsTr("Show observation intervals, total events, follow-up time and observed cumulative mean time between events for each subgroup.")
			}
		]
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
		columns: 2
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
		SetSeed
		{
			title: qsTr("Reproducibility")
			enabled: model.value === "piecewise" || diagnostics.quantileSelected || growthExport.quantileSelected
		}
	}
}
