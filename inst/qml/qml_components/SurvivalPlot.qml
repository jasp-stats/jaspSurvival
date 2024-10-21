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


import QtQuick 2.8
import JASP.Controls 1.0

CheckBox
{
	name:	"plot"
	label:	qsTr("Plot")

	DropDown
	{
		name:		"plotType"
		label:		qsTr("Type")
		values:
		[
			{ label: qsTr("Survival"),					value: "survival"},
			{ label: qsTr("Risk"),						value: "risk"},
			{ label: qsTr("Cumulative hazard"),			value: "cumulativeHazard"},
			{ label: qsTr("Complementary log-log"),		value: "complementaryLogLog"}
		]
	}

	CheckBox
	{
		name:		"plotCi"
		label:		qsTr("Confidence interval")
		checked:	true
	}

	CheckBox
	{
		name:		"plotRiskTable"
		label:		qsTr("Risk table")
		checked:	false

		CheckBox
		{
			name:		"plotRiskTableNumberAtRisk"
			label:		qsTr("Number at risk")
			checked:	true
		}

		CheckBox
		{
			name:		"plotRiskTableCumulativeNumberOfObservedEvents"
			label:		qsTr("Cum. number of observed events")
			checked:	true
		}

		CheckBox
		{
			name:		"plotRiskTableCumulativeNumberOfCensoredObservations"
			label:		qsTr("Cum. number of censored obs.")
		}

		CheckBox
		{
			name:		"plotRiskTableNumberOfEventsInTimeInterval"
			label:		qsTr("Number of events in time interval")
		}

		CheckBox
		{
			name:		"plotRiskTableNumberOfCensoredObservationsInTimeInterval"
			label:		qsTr("Number of censored obs. in time interval")
		}

		CheckBox
		{
			name:		"plotRiskTableAsASingleLine"
			label:		qsTr("As a single line")
		}
	}

	CheckBox
	{
		name:		"plotAddQuantile"
		label:		qsTr("Add quantile")
		checked:	false
		childrenOnSameRow:	true
		
		DoubleField
		{
			name:			"plotAddQuantileValue"
			defaultValue:	0.5	
			min:			0
			max:			1
		}
	}


	DropDown
	{
		name:		"plotLegend"
		enabled:	strata.count > 0
		label:		qsTr("Legend")
		values:
		[
			{ label: qsTr("Bottom"),	value: "bottom"},
			{ label: qsTr("Right"),		value: "right"},
			{ label: qsTr("Left"),		value: "left"},
			{ label: qsTr("Top"),		value: "top"},
			{ label: qsTr("None"),		value: "none"}
		]
	}

	ColorPalette
	{
		enabled:	strata.count > 0
	}

	DropDown
	{
		name:		"plotTheme"
		label:		qsTr("Theme")
		values:
		[
			{ label: qsTr("JASP"),			value: "jasp"},
			{ label: qsTr("ggsurvfit"),		value: "ggsurvfit"}
		]
	}
}
