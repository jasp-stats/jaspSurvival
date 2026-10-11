pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

// Native FactorsForm layout with model-count ownership in the location form.
FactorsFormBase
{
	id: factorsForm
	property var locationModels
	property string availableVariablesListName: ""
	property alias availableVariablesList: availableVariablesList
	property alias keepAvailableVariables: availableVariablesList.keepVariablesWhenMoved
	property var allowedColumns: []
	property int listWidth: parent.width * 2 / 5
	property int factorListHeight: jaspTheme.defaultVariablesFormHeight / 3 - factorsFormColumn.spacing
	implicitHeight: jaspTheme.defaultVariablesFormHeight + Math.max((factorsFormRepeater.count - 3), 0) * (factorListHeight + factorsFormColumn.spacing)
	implicitWidth: jaspForm.width
	height: implicitHeight
	width: implicitWidth
	Layout.columnSpan: parent.columns
	optionKey: "components"
	startIndex: 1
	initNumberFactors: 1
	allowInteraction: true
	baseTitle: qsTr("Model")

	function queueSynchronization() { Qt.callLater(synchronize); }
	function synchronize()
	{
		if (!initialized || !locationModels || !locationModels.initialized) return;
		// Repeater rows must be registered before the native last-row operations.
		for (let i = 0; i < factorsFormRepeater.count; ++i)
			if (!factorsFormRepeater.itemAt(i) || !factorsFormRepeater.itemAt(i).registered) return;
		if (count < locationModels.count) { addFactor(); return; }
		if (count > locationModels.count) { removeFactor(); queueSynchronization(); return; }
		for (let i = 0; i < count; ++i)
		{
			let title = locationModels.factorsTitles[i].label;
			if (factorsTitles[i].label !== title) titleChanged(i, title);
		}
	}
	onInitializedChanged: queueSynchronization()
	onFactorsTitlesChanged: queueSynchronization()
	Connections
	{
		target: factorsForm.locationModels
		function onFactorsTitlesChanged() { factorsForm.queueSynchronization(); }
		function onInitializedChanged() { factorsForm.queueSynchronization(); }
	}
	AvailableVariablesList
	{
		id: availableVariablesList
		name: factorsForm.availableVariablesListName
		width: factorsForm.listWidth
		preferredHeight: parent.height
		anchors.top: parent.top
		anchors.left: parent.left
	}
	Column
	{
		id: factorsFormColumn
		spacing: 10
		width: parent.width * 3 / 5
		anchors.top: parent.top
		anchors.right: parent.right
		Repeater
		{
			id: factorsFormRepeater
			model: factorsForm.model
			RowLayout
			{
				property alias factorList: factorList
				property alias button: button
				property bool registered: false
				property bool isDynamic: true
				spacing: 0
				AssignButton
				{
					id: button
					name: "Factor form"
					Layout.leftMargin: (factorsFormColumn.width / 3 - width) / 2
					Layout.rightMargin: (factorsFormColumn.width / 3 - width) / 2
					leftSource: factorsForm.availableVariablesList
					rightSource: factorList
					Component.onDestruction: availableVariablesList.activeFocusChanged.disconnect(button.setIconToRight)
				}
				FactorsList
				{
					id: factorList
					name: factorName
					editableTitle: factorTitle
					dropKeys: factorsForm.availableVariablesListName
					allowedColumns: factorsForm.allowedColumns
					implicitHeight: factorsForm.factorListHeight
					implicitWidth: factorsForm.listWidth
					isBound: false
					listViewType: JASP.Interaction
					onTitleIsChanged: factorsForm.titleChanged(index, editableTitle)
				}
			}
			onItemAdded: (index, item) =>
			{
				availableVariablesList.dropKeys.push(item.factorList.name);
				for (let i = 0; i < index; ++i)
				{
					itemAt(i).factorList.dropKeys.push(item.factorList.name);
					item.factorList.dropKeys.push(itemAt(i).factorList.name);
				}
				item.factorList.activeFocusChanged.connect(item.button.setIconToLeft);
				availableVariablesList.activeFocusChanged.connect(item.button.setIconToRight);
				factorsForm.factorAdded(index, item.factorList);
				item.registered = true;
				factorsForm.queueSynchronization();
			}
			onItemRemoved: factorsForm.queueSynchronization()
		}
	}
}
