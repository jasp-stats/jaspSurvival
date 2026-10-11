import QtQuick
import JASP.Module

ChangeJS
{
	id: newOption
	isNewOption: true
	condition: function(options) { return options[newOption.name] === undefined; }
}
