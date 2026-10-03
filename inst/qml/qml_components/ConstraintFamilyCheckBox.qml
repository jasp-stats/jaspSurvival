//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import JASP.Controls

CheckBox
{
	property bool constraintActive: false
	property bool constraintStateInitialized: false
	property bool hasPreviousSelection: false
	property bool previousChecked: false

	enabled: !constraintActive

	onConstraintActiveChanged:
	{
		if (!constraintStateInitialized)
			return

		if (constraintActive)
		{
			previousChecked = checked
			hasPreviousSelection = true
			checked = false
		}
		else if (hasPreviousSelection)
		{
			checked = previousChecked
			hasPreviousSelection = false
		}
	}

	onCheckedChanged: if (constraintStateInitialized && constraintActive && checked) checked = false

	Component.onCompleted:
	{
		constraintStateInitialized = true
		if (constraintActive)
			checked = false
	}
}
