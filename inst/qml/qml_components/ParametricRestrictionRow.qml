import QtQuick

// C++ creates and owns each row control. Reparent it into this display delegate
// instead of creating another control with a separate saved-option binding.
FocusScope
{
	id: rowWrapper
	property var rowComponentItem
	width: rowComponentItem ? rowComponentItem.width : 0
	height: rowComponentItem ? rowComponentItem.height : 0
	Component.onCompleted:
	{
		if (rowComponentItem)
		{
			rowComponentItem.parent = rowWrapper;
			rowComponentItem.anchors.left = rowWrapper.left;
		}
	}
}
