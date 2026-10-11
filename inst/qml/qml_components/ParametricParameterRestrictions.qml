pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

ParametricRestrictionSlots
{
	id: restrictions
	property var selector
	property real rowSpacing: 4 * jaspTheme.uiScale
	property bool addBorder: true
	property alias itemRectangle: frame
	readonly property real distributionWidth: 190 * jaspTheme.uiScale
	readonly property real componentWidth: 60 * jaspTheme.uiScale
	readonly property var activeFamilies: ["all", "bestAic", "bestBic"].indexOf(selector.value) >= 0 ?
		selector.selectedFamilies.map(function(family) { return family.value; }) : [selector.value]
	name: "restrictParametersValues"
	optionKey: "restriction"
	visible: selector.parametersRestricted
	enabled: selector.parametersRestricted
	Layout.columnSpan: form.columns
	Layout.fillWidth: true
	implicitWidth: rows.childrenRect.width + 2 * jaspTheme.contentMargin
	implicitHeight: rows.height + 2 * jaspTheme.contentMargin
	preferredWidth: parent.width
	preferredHeight: implicitHeight
	background: frame
	innerControl: rows
	shouldStealHover: false

	Component.onCompleted:
	{
		let keys = [];
		for (let family of selector.familyChoices)
			for (let k = 1; k <= 4; k++)
				keys.push(family.value + ":" + k);
		restrictionKeys = keys;
	}

	Rectangle
	{
		id: frame
		anchors.fill: parent
		color: jaspTheme.controlBackgroundColor
		border.width: restrictions.addBorder ? 1 : 0
		border.color: jaspTheme.borderColor
		radius: jaspTheme.borderRadius
	}
	Column
	{
		id: rows
		width: parent.width - 2 * jaspTheme.contentMargin
		x: jaspTheme.contentMargin
		y: jaspTheme.contentMargin
		spacing: restrictions.rowSpacing
		RowLayout
		{
			spacing: 8 * jaspTheme.uiScale
			Label
			{
				text: qsTr("Distribution")
				Layout.preferredWidth: restrictions.distributionWidth
			}
			Label
			{
				text: qsTr("Component")
				visible: restrictions.selector.mixture
				Layout.preferredWidth: restrictions.componentWidth
			}
			Label { text: qsTr("Parameters") }
		}
		Repeater
		{
			model: restrictions.model
			delegate: ParametricRestrictionRow
			{
				rowComponentItem: model.rowComponent
				readonly property var specification: String(model.value).split(":")
				visible: restrictions.activeFamilies.indexOf(specification[0]) >= 0 && Number(specification[1]) <= restrictions.selector.componentCount
			}
		}
	}
	rowComponent: RowLayout
	{
		id: restrictionRow
		readonly property var specification: rowValue.split(":")
		readonly property string distribution: specification[0]
		readonly property int component: Number(specification[1])
		readonly property string distributionLabel: listView.selector.familyChoices.find(function(family) {
			return family.value === restrictionRow.distribution;
		}).label
		readonly property bool mixture: listView.selector.mixture
		spacing: 8 * jaspTheme.uiScale
		Group
		{
			name: "distributionColumn"
			Layout.preferredWidth: listView.distributionWidth
			Label
			{
				text: restrictionRow.component === 1 ? restrictionRow.distributionLabel : ""
			}
		}
		Group
		{
			name: "componentColumn"
			visible: restrictionRow.mixture
			Layout.preferredWidth: visible ? listView.componentWidth : 0
			Label
			{
				text: restrictionRow.component
				visible: restrictionRow.mixture
			}
		}
		Group
		{
			name: "parametersColumn"
			ParametricFixedParameterFields { distribution: restrictionRow.distribution }
		}
	}
}
