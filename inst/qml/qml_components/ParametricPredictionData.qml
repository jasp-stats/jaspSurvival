pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Controls as QtControls
import QtQuick.Layouts
import JASP.Controls
import JASP

BasicThreeButtonTableView
{
	id: observations
	property var predictionPredictors: []
	property var factorPredictors: []
	property var disabledPredictors: []
	property bool singleRow: false
	property bool predictorsSourceReady: false
	property bool mappingAdopted: false
	property bool schemaError: false
	property var previousPredictors: []
	property int factorLevelsRevision: 0

	// Level sources live outside the table's dynamic cell context so JASP initializes them.
	Repeater
	{
		id: factorSources
		model: observations.factorPredictors
		delegate: AvailableVariablesList
		{
			isBound: false
			visible: false
			source: [{values: [modelData], use: "levels"}]
			onColumnsNamesChanged: observations.factorLevelsRevision++
		}
		onItemAdded: observations.factorLevelsRevision++
		onItemRemoved: observations.factorLevelsRevision++
	}
	function levelsFor(predictor)
	{
		let index = factorPredictors.indexOf(predictor);
		let source = index >= 0 && index < factorSources.count ? factorSources.itemAt(index) : null;
		return source ? source.columnsNames : [];
	}

	name: "predictionsManualInput"
	modelType: JASP.Simple
	itemType: JASP.String
	initialColumnCount: Math.max(1, predictionPredictors.length)
	initialRowCount: 1
	columnNames: mappingAdopted && !schemaError ? (previousPredictors.length > 0 ? previousPredictors : [qsTr("Observation")]) : []
	defaultValue: ""
	tableView.parseDefaultValue: false
	tableView.showAddRemoveButtons: false
	tableView.itemDelegate: Item
	{
		id: predictionCell
		readonly property string predictor: observations.previousPredictors[columnIndex] || ""
		readonly property bool factor: observations.factorPredictors.indexOf(predictor) >= 0
		readonly property string cellValue: String(itemText)
		readonly property bool cellEditable: observations.tableView.enabled && itemEditable && observations.getEditable(columnIndex, rowIndex)

		QtControls.ComboBox
		{
			visible: predictionCell.factor
			enabled: predictionCell.cellEditable
			anchors.verticalCenter: parent.verticalCenter
			width: parent.width
			height: jaspTheme.comboBoxHeight
			font: jaspTheme.font
			implicitContentWidthPolicy: QtControls.ComboBox.WidestText
			popup.width: Math.max(width, implicitContentWidth + leftPadding + rightPadding)
			model:
			{
				observations.factorLevelsRevision;
				return [""].concat(observations.levelsFor(predictionCell.predictor));
			}
			currentIndex: model.indexOf(predictionCell.cellValue)
			// Only user selections change the table; loading or refreshing levels must not overwrite saved cells.
			onActivated: function(index)
			{
				observations.tableView.itemChanged(columnIndex, rowIndex, model[index], "string");
			}
		}
		FormulaField
		{
			visible: !predictionCell.factor
			inputType: "string"
			isBound: false
			anchors.left: parent.left
			anchors.top: parent.top
			anchors.topMargin: -8 * jaspTheme.uiScale
			fieldHeight: parent.height + 16 * jaspTheme.uiScale
			fieldWidth: parent.width
			useExternalBorder: false
			showBorder: false
			value: itemText
			useLastValidValue: false
			parseDefaultValue: observations.tableView.parseDefaultValue
			defaultValue: observations.getDefaultValue(columnIndex, rowIndex)
			selectValueOnFocus: true
			validator: observations.getValidator(columnIndex, rowIndex)
			editable: predictionCell.cellEditable
			onPressed: observations.tableView.colSelected = columnIndex
			onEditingFinished: observations.tableView.itemChanged(columnIndex, rowIndex, displayValue, inputType)
		}
	}
	buttonsInRow: true
	showButtons: false
	Layout.minimumWidth: singleRow ? 100 * jaspTheme.uiScale : observationButtons.implicitWidth
	tableView.anchors.top: singleRow ? observations.top : observationButtons.bottom
	tableView.anchors.topMargin: jaspTheme.generalAnchorMargin
	preferredHeight: Math.min(300 * jaspTheme.uiScale, tableView.y + tableView.tableHeight)
	buttonAddText: qsTr("Add observation")
	buttonDeleteText: qsTr("Delete observation")
	buttonDeleteEnabled: tableView.rowCount > 1
	onAddClicked: if (!singleRow) tableView.addRow()
	onDeleteClicked: if (!singleRow) tableView.removeARow()
	onResetClicked:
	{
		tableView.reset();
		schemaError = false;
		tableView.clearControlError();
		mappingAdopted = false;
		queueSynchronize();
	}
	info: qsTr("Enter predictor values for one observation per row. Leave cells empty to use the sample mean or factor reference level.")

	RowLayout
	{
		id: observationButtons
		visible: !observations.singleRow
		width: parent.width
		spacing: jaspTheme.columnGroupSpacing
		RoundedButton
		{
			text: observations.buttonAddText
			Layout.fillWidth: true
			Layout.preferredWidth: 1
			Layout.minimumWidth: implicitWidth
			onClicked: { forceActiveFocus(); observations.addClicked(); }
		}
		RoundedButton
		{
			text: observations.buttonDeleteText
			enabled: observations.buttonDeleteEnabled
			Layout.fillWidth: true
			Layout.preferredWidth: 1
			Layout.minimumWidth: implicitWidth
			onClicked: { forceActiveFocus(); observations.deleteClicked(); }
		}
		RoundedButton
		{
			text: observations.buttonResetText
			enabled: observations.buttonResetEnabled
			Layout.fillWidth: true
			Layout.preferredWidth: 1
			Layout.minimumWidth: implicitWidth
			onClicked: { forceActiveFocus(); observations.resetClicked(); }
		}
	}

	function getEditable(columnIndex, rowIndex)
	{
		let predictor = previousPredictors[columnIndex];
		return mappingAdopted && !schemaError && columnIndex >= 0 && columnIndex < previousPredictors.length && predictionPredictors.indexOf(predictor) >= 0 && disabledPredictors.indexOf(predictor) < 0;
	}
	function queueSynchronize() { Qt.callLater(synchronizeColumns); }
	function samePredictors(a, b) { return a.length === b.length && a.every(function(v, i) { return v === b[i]; }); }

	// Keep observations attached to their predictor when columns are added or reordered.
	function synchronizeColumns()
	{
		if (!tableView.initialized || !predictorsSourceReady || schemaError) return;
		if (mappingAdopted && samePredictors(previousPredictors, predictionPredictors)) return;
		let count = Math.max(1, predictionPredictors.length);
		if (!mappingAdopted)
		{
			// Saved native headers are positional, not predictor identities. Adopt only a stable schema.
			if (tableView.columnCount !== count)
			{
				for (let col = 0; col < tableView.columnCount; ++col)
					for (let row = 0; row < tableView.rowCount; ++row)
						if (String(tableView.model.data(tableView.model.index(row, col), Qt.DisplayRole)) !== "")
						{
							schemaError = true;
							tableView.addControlErrorPermanent(qsTr("The saved manual input columns do not match the current predictors. Review the saved values, then reset the table and re-enter them for the current predictors."));
							return;
						}
				tableView.columnCount = count;
			}
			previousPredictors = predictionPredictors.slice();
			mappingAdopted = true;
			return;
		}
		let values = [];
		for (let col = 0; col < previousPredictors.length; col++)
		{
			let column = [];
			for (let row = 0; row < tableView.rowCount; row++)
				column.push(tableView.model.data(tableView.model.index(row, col), Qt.DisplayRole));
			values.push(column);
		}
		if (tableView.columnCount !== count) tableView.columnCount = count;
		for (let col = 0; col < tableView.columnCount; col++)
		{
			let previous = previousPredictors.indexOf(predictionPredictors[col]);
			for (let row = 0; row < tableView.rowCount; row++)
			{
				let desired = previous < 0 ? "" : String(values[previous][row]);
				let current = col < values.length ? String(values[col][row]) : "";
				if (current !== desired) tableView.itemChanged(col, row, desired, "string");
			}
		}
		previousPredictors = predictionPredictors.slice();
	}
	onPredictionPredictorsChanged: queueSynchronize()
	onPredictorsSourceReadyChanged: queueSynchronize()
	onTableViewCompleted: queueSynchronize()
	property Connections tableConnections: Connections
	{
		target: observations.tableView
		function onInitializedChanged() { observations.queueSynchronize(); }
	}
}
