pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import JASP.Controls
import JASP

ParametricRestrictionSlots
{
	id: restrictions
	property int componentCount: 1
	property real rowSpacing: 4 * jaspTheme.uiScale
	readonly property real rowHeight: jaspTheme.comboBoxHeight
	optionKey: "component"
	implicitWidth: componentRows.width
	implicitHeight: componentRows.height
	preferredWidth: implicitWidth
	preferredHeight: implicitHeight
	restrictionKeys: ["1", "2", "3", "4"]
	rowComponent: ParametricFixedParameter
	{
		readonly property int component: Number(rowValue)
		name: "parameterRestriction"
		positive: false
		fieldWidth: 125 * jaspTheme.uiScale
		fieldHeight: listView.rowHeight
		visible: component <= listView.componentCount
		info: qsTr("Fix this component's regression coefficients at specified values. Leave empty to estimate them; separate multiple values with commas or semicolons.")
	}
	Column
	{
		id: componentRows
		spacing: restrictions.rowSpacing
		Repeater
		{
			model: restrictions.model
			delegate: ParametricRestrictionRow
			{
				rowComponentItem: model.rowComponent
				visible: rowComponentItem && rowComponentItem.component <= restrictions.componentCount
			}
		}
	}
}
