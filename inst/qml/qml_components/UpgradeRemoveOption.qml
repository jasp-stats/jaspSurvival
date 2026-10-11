import QtQuick
import JASP.Module

ChangeRemove
{
	id: removeOption
	condition: function(options) { return options[removeOption.name] !== undefined; }
}
