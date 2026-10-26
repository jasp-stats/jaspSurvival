import QtQuick

// Follow JASP ComponentsList's delegate: C++ owns the saved row control;
// reparent its visual item instead of creating a second option binding.
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
