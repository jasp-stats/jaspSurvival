pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	id: roles
	property var analysisForm
	property var predictors: []
	property var covariates: []
	property var factors: []
	property bool sourcesReady: false
	property bool applying: false
	property var previousAutomatic: null
	property var projection: null
	readonly property var typedSources: [
		{name: "covariates", discard: [{values: covariates.filter(function(v) { return predictors.indexOf(v) < 0; })}]},
		{name: "factors", discard: [{values: factors.filter(function(v) { return predictors.indexOf(v) < 0; })}]}
	]
	readonly property var axisVariables: names(axis.columnsNames)
	readonly property var lineVariables: names(lines.columnsNames)
	readonly property var panelVariables: names(panels.columnsNames)
	readonly property string axisVariable: axis.count ? axis.columnsNames[0] : ""
	readonly property bool automaticAssignments: automatic.checked
	readonly property bool continuousGroups: lines.columnsTypes.indexOf("scale") >= 0 || panels.columnsTypes.indexOf("scale") >= 0
	readonly property bool axisIsTime: axisVariables.length === 1 && axisVariable === timeId.value
	readonly property bool axisIsCategorical: axisVariables.length > 0 && axisVariables.every(function(v) { return factors.indexOf(v) >= 0; })
	function names(sequence)
	{
		let result = [];
		for (let i = 0; i < sequence.length; ++i) result.push(sequence[i]);
		return result;
	}
	function same(a, b) { return a.length === b.length && a.every(function(v, i) { return v === b[i]; }); }
	function queueSync() { Qt.callLater(synchronize); }
	function explicitEdit()
	{
		if (applying || !automatic.initialized) return;
		automatic.checked = false;
	}
	function removeOtherNames(control, desired)
	{
		let current = names(control.columnsNames);
		for (let i = current.length - 1; i >= 0; --i)
			if (desired.indexOf(current[i]) < 0 || current.indexOf(current[i]) !== i) control.itemDoubleClicked(i);
	}
	function finishMove() { applying = false; queueSync(); }
	function synchronize()
	{
		if (applying || !sourcesReady || !analysisForm || !axis.initialized || !lines.initialized || !panels.initialized || !timeId.initialized || !automatic.initialized || !inventory.initialized || !available.initialized) return;
		applying = true;
		if (!projection)
		{
			let id = timeId.value;
			let oldId = id;
			let x = names(axis.columnsNames);
			let lineValues = names(lines.columnsNames);
			let panelValues = names(panels.columnsNames);
			// Plain native assignments save values, so the synthetic item's value is its label.
			let privateUse = /[\uE000-\uF8FF]/.test(id) || Array.from(id).some(function(v) { return v.codePointAt(0) >= 0xF0000; });
			if (id === qsTr("Lifetime quantile / Time") || id === qsTr("Lifetime quantile / Time (prediction)") || !id || privateUse || id === qsTr("Time") || id === qsTr("Time (prediction)") || inventory.columnsNames.indexOf(id) >= 0)
			{
				let used = names(inventory.columnsNames);
				id = qsTr("Lifetime / Time");
				if (used.indexOf(id) >= 0) id = qsTr("Lifetime / Time (prediction)");
				let base = id;
				for (let suffix = 2; used.indexOf(id) >= 0; ++suffix) id = base + " " + suffix;
				// Preserve a real dataset column with the old name; rotate only unknown Time.
				let saved = analysisForm.options();
				let rotate = function(control, values) {
					let option = saved[control.name];
					let types = option && option.types;
					return values.map(function(v, i) { return v === oldId && (!types || (typeof types === "string" ? types : types[i]) === "unknown") ? id : v; });
				};
				x = rotate(axis, x);
				lineValues = rotate(lines, lineValues);
				panelValues = rotate(panels, panelValues);
			}
			let factorDefaults = predictors.filter(function(v) { return factors.indexOf(v) >= 0; });
			let projected = previousAutomatic ? {
				x: previousAutomatic.x.map(function(v) { return v === oldId ? id : v; }),
				lines: previousAutomatic.lines.filter(function(v) { return predictors.indexOf(v) >= 0; }),
				panels: previousAutomatic.panels.filter(function(v) { return predictors.indexOf(v) >= 0; })
			} : null;
			let defaultSaved = (x.length === 0 || (x.length === 1 && x[0] === id)) && panelValues.length === 0 && (lineValues.length === 0 || (lineValues.length === factorDefaults.length && lineValues.every(function(v) { return factorDefaults.indexOf(v) >= 0; })));
			if (automatic.checked && (projected ? !(same(x, projected.x) && same(lineValues, projected.lines) && same(panelValues, projected.panels)) : !defaultSaved)) automatic.checked = false;
			if (automatic.checked) { x = [id]; lineValues = factorDefaults; panelValues = []; }
			// Native source removal and saved/RPC overlaps project to exclusive current roles.
			x = x.filter(function(v, i, a) { return (v === id || predictors.indexOf(v) >= 0) && a.indexOf(v) === i; });
			if (x.length > 1) x = x[x.length - 1] === id ? [id] : x.filter(function(v) { return v !== id; });
			lineValues = lineValues.filter(function(v, i, a) { return (v === id || predictors.indexOf(v) >= 0) && x.indexOf(v) < 0 && a.indexOf(v) === i; });
			panelValues = panelValues.filter(function(v, i, a) { return (v === id || predictors.indexOf(v) >= 0) && x.indexOf(v) < 0 && lineValues.indexOf(v) < 0 && a.indexOf(v) === i; });
			if (x.indexOf(id) < 0 && lineValues.indexOf(id) < 0 && panelValues.indexOf(id) < 0) lineValues.push(id);
			projection = {x: x, lines: lineValues, panels: panelValues};
			if (timeId.value !== id) timeId.value = id;
		}
		// A predictor may disappear while a native transfer is queued.
		let valid = function(v) { return v === timeId.value || predictors.indexOf(v) >= 0; };
		projection = {x: projection.x.filter(valid), lines: projection.lines.filter(valid), panels: projection.panels.filter(valid)};
		if (projection.x.indexOf(timeId.value) < 0 && projection.lines.indexOf(timeId.value) < 0 && projection.panels.indexOf(timeId.value) < 0) projection.lines.push(timeId.value);
		let targets = [{control: axis, values: projection.x}, {control: lines, values: projection.lines}, {control: panels, values: projection.panels}];
		for (let target of targets) removeOtherNames(target.control, target.values);
		for (let target of targets)
		{
			let current = names(target.control.columnsNames);
			let missing = target.values.filter(function(v) { return current.indexOf(v) < 0; });
			if (!missing.length) continue;
			let indices = [];
			for (let i = 0; i < available.columnsNames.length; ++i)
				if (missing.indexOf(available.columnsNames[i]) >= 0) indices.push(i);
			if (indices.length) { available.itemsDropped(indices, target.control, -1); Qt.callLater(finishMove); return; }
			// Source notifications resume an identifier rotation once its item is available.
			applying = false;
			return;
		}
		if (automatic.checked) previousAutomatic = {x: names(axis.columnsNames), lines: names(lines.columnsNames), panels: names(panels.columnsNames)};
		projection = null;
		applying = false;
	}
	onPredictorsChanged: queueSync()
	onFactorsChanged: queueSync()
	onSourcesReadyChanged: queueSync()
	AvailableVariablesList
	{
		id: inventory
		name: "predictionsDatasetInventory"
		visible: false
		onColumnsNamesChanged: roles.queueSync()
		onInitializedChanged: roles.queueSync()
	}
	TextField
	{
		id: timeId
		name: "predictionsLifetimeTimeId"
		defaultValue: qsTr("Lifetime / Time")
		visible: false
		onInitializedChanged: roles.queueSync()
		onBoundValueChanged: roles.queueSync()
	}
	CheckBox
	{
		id: automatic
		name: "predictionsRolesAutomatic"
		checked: true
		visible: false
		onInitializedChanged: roles.queueSync()
	}
	VariablesForm
	{
		preferredHeight: 270 * jaspTheme.uiScale
		AvailableVariablesList
		{
			id: available
			name: "predictionsModelVariables"
			title: qsTr("Model variables")
			source: roles.typedSources.concat([{values: [{label: timeId.value, value: timeId.value}]}])
			onItemsDropped: roles.explicitEdit()
			onItemDoubleClicked: roles.explicitEdit()
			onInitializedChanged: roles.queueSync()
			onColumnsNamesChanged: roles.queueSync()
		}
		AssignedVariablesList
		{
			id: axis
			name: "predictionsXAxisVariables"
			title: qsTr("X-axis variables")
			allowedColumns: ["scale", "nominal"]
			allowTypeChange: false
			onItemsDropped: roles.explicitEdit()
			onItemDoubleClicked: roles.explicitEdit()
			onInitializedChanged: roles.queueSync()
			onBoundValueChanged: roles.queueSync()
			info: qsTr("Select predictors for the x-axis, or choose Lifetime / Time to plot predictions over percentiles or time.")
		}
		AssignedVariablesList
		{
			id: lines
			name: "predictionsSeparateLines"
			title: qsTr("Separate lines")
			allowedColumns: ["scale", "nominal"]
			allowTypeChange: false
			onItemsDropped: roles.explicitEdit()
			onItemDoubleClicked: roles.explicitEdit()
			onInitializedChanged: roles.queueSync()
			onBoundValueChanged: roles.queueSync()
			info: qsTr("Draw separate lines for predictor levels or groups, or for the selected times or percentiles.")
		}
		AssignedVariablesList
		{
			id: panels
			name: "predictionsSeparatePlots"
			title: qsTr("Separate plots")
			allowedColumns: ["scale", "nominal"]
			allowTypeChange: false
			onItemsDropped: roles.explicitEdit()
			onItemDoubleClicked: roles.explicitEdit()
			onInitializedChanged: roles.queueSync()
			onBoundValueChanged: roles.queueSync()
			info: qsTr("Create separate plots for predictor levels or groups, or for the selected times or percentiles.")
		}
	}
}
