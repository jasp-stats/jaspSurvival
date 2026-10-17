import QtQuick
import JASP.Controls

// Stable keys retain edits when displayed families or component counts change.
// The counter changes only internal source labels to populate slots absent from
// saved options; the derived controls provide their own visible labels.
ComponentsListBase
{
	id: slots
	property var restrictionKeys: []
	property int sourceGeneration: 0
	addItemManually: false
	source: [{values: restrictionKeys.map(function(key) {
		return {value: key, label: sourceGeneration + ":" + key};
	})}]

	onCountChanged: if (count < restrictionKeys.length) Qt.callLater(restoreMissingSlots)
	function restoreMissingSlots()
	{
		if (count < restrictionKeys.length) sourceGeneration++;
	}
}
