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
// Preserve the shared form's existing translation context.
pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	property bool mixture: false
	readonly property bool rightCensoring: censoringTypeRight.checked
	readonly property bool intervalCensoring: censoringTypeInterval.checked
	readonly property bool countingCensoring: censoringTypeCounting.checked

	RadioButtonGroup
	{
		id:						censoringType
		Layout.columnSpan:		1
		name:					"censoringType"
		title:					qsTr("Censoring Type")
		radioButtonsOnSameRow:	true
		columns:				3
		info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Select right-censored data, counting-process data with entry and exit times, or interval-censored data (including left-censored observations).") : qsTr("Select right-censored data, counting-process data with delayed entry, or interval-censored data.")

		RadioButton
		{
			label:		qsTr("Right")
			value:		"right"
			id:			censoringTypeRight
			checked:	true
		}

		RadioButton
		{
			label:		qsTr("Counting")
			value:		"counting"
			id:			censoringTypeCounting
		}

		RadioButton
		{
			label:		qsTr("Interval")
			value:		"interval"
			id:			censoringTypeInterval
			info: qsTr("If interval censoring is selected, the following coding needs to be used: left-censored data is represented as (NA, t2), right-censored data as (t1, NA), exact data as (t, t), and interval-censored data as (t1, t2).")
		}
	}

	CheckBox
	{
		name:		"censoringSummary"
		label:		qsTr("Censoring summary")
		info: qsTr("Create a summary table with information about the censoring status of the data.")
	}
}
