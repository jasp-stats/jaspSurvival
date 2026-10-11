import QtQuick
import JASP.Module

ChangeRename
{
	id: renameOption
	condition: function(options) { return options[renameOption.from] !== undefined && options[renameOption.to] === undefined; }
}
