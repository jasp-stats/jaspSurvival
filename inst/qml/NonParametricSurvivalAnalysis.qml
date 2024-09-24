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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form
{
	VariablesForm
	{
		AvailableVariablesList
		{
			name: "allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"timeToEvent"
			title:				qsTr("Time to Event")
			allowedColumns:		["scale"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			id:					eventStatusId
			name:				"eventStatus"
			title:				qsTr("Event Status")
			allowedColumns:		["nominal"]
			singleVariable:		true
		}

		DropDown
		{
			name:				"eventIndicator"
			label:				qsTr("Event Indicator")
			source:				[{name: "eventStatus", use: "levels"}]
			onCountChanged:		currentIndex = 1
		}

		AssignedVariablesList
		{
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["nominal"]
		}
	}

	Group
	{

		Group
		{
			title:	qsTr("Tests")

			CheckBox
			{
				name:		"testsLogRank"
				label:		qsTr("Log-rank (Mantel-Haenszel)")
			}

			CheckBox
			{
				name:		"testsPetoAndPeto"
				label:		qsTr("Peto and Peto")
			}

			CheckBox
			{
				name:		"testsFlemmingHarrington"
				label:		qsTr("Flemming-Harrington")

				DoubleField
				{
					name:			"testsFlemmingHarringtonRho"
					label:			qsTr("Rho")
					defaultValue:	0.5
					min:			0
					max:			1
				}
			}
		}

		CheckBox
		{
			name:	"lifeTable"
			label:	qsTr("Life table")

			DropDown
			{
				name:		"lifeTableStepsType"
				id:			lifeTableStepsType
				label:		qsTr("Steps type")
				values:
				[
					{ label: qsTr("Default"),		value: "default"},
					{ label: qsTr("Quantilies"),	value: "quantiles"},
					{ label: qsTr("Fixed size"),	value: "fixedSize"}
				]
			}

			IntegerField
			{
				name:			"lifeTableStepsNumber"
				label:			qsTr("Number")
				defaultValue:	10
				visible:		lifeTableStepsType.value == "quantiles"
			}

			CheckBox
			{
				name:		"lifeTableRoundSteps"
				label:		qsTr("Round steps")
				checked:	true
				visible:	lifeTableStepsType.value == "quantiles"
			}

			DoubleField
			{
				name:			"lifeTableStepsFrom"
				id:				lifeTableStepsFrom
				label:			qsTr("From")
				defaultValue:	0
				max:			lifeTableStepsTo.value
				visible:		lifeTableStepsType.value == "fixedSize"
			}

			DoubleField
			{
				name:			"lifeTableStepsSize"
				label:			qsTr("Size")
				defaultValue:	1
				// max:			lifeTableStepsTo.value // TODO: enable once max is data dependent
				visible:		lifeTableStepsType.value == "fixedSize"
			}

			DoubleField
			{
				name:			"lifeTableStepsTo"
				id:				lifeTableStepsTo
				label:			qsTr("To")
				defaultValue:	0
				min:			lifeTableStepsFrom.value
				visible:		lifeTableStepsType.value == "fixedSize"
			}
		}
	}

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
			name:		"plotConfidenceInterval"
			label:		qsTr("Confidence interval")
			checked:	true
		}

		CheckBox
		{
			name:		"plotRiskTable"
			label:		qsTr("Risk table")
			checked:	true
			childrenOnSameRow:	true

			DropDown
			{
				name:		"plotRiskTableValue"
				values:
				[
					{ label: qsTr("Number at risk"),	value: "numberAtRisk"},
					{ label: qsTr("Cumulative events"),	value: "cumulativeEvents"},
					{ label: qsTr("Both"),				value: "both"},
					{ label: qsTr("Both (brackets)"),	value: "bothBrackets"}
				]
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
		
		ColorPalette{}

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
}
