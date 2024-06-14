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

		RadioButtonGroup
		{
			id:						censoringType
			Layout.columnSpan:		2
			name:					"censoringType"
			title:					qsTr("Censoring Type")
			radioButtonsOnSameRow:	true
			columns:				2

			RadioButton
			{
				label:		qsTr("Right")
				value:		"right"
				id:			censoringTypeRight
			}

			RadioButton
			{
				label:		qsTr("Interval")
				value:		"interval"
				id:			censoringTypeInterval
			}
		}
/*
		AssignedVariablesList
		{
			name:				"intervalStart"
			title:				qsTr("Interval Start")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeInterval.checked
		}

		AssignedVariablesList
		{
			name:				"intervalEnd"
			title:				qsTr("intervalEnd")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeInterval.checked
		}
*/
		AssignedVariablesList
		{
			name:				"timeToEvent"
			title:				qsTr("Time to Event")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeRight.checked	
		}

		AssignedVariablesList
		{
			name:				"eventStatus"
			title:				qsTr("Event Status")
			allowedColumns:		["nominal"]
			singleVariable:		true
		}
/*
		DropDown
		{
			name:				"rightCensored"
			label:				qsTr("Right Censored")
			source:				[{name: "eventStatus", use: "levels"}]
			visible:			censoringTypeInterval.checked
		}
*/
		DropDown
		{
			name:				"eventIndicator"
			label:				qsTr("Event Indicator")
			source:				[{name: "eventStatus", use: "levels"}]
		}
/*
		DropDown
		{
			name:				"leftCensored"
			label:				qsTr("Left Censored")
			source:				[{name: "eventStatus", use: "levels"}]
			visible:			censoringTypeInterval.checked
		}

		DropDown
		{
			name:				"intervalCensored"
			label:				qsTr("Interval Censored")
			source:				[{name: "eventStatus", use: "levels"}]
			visible:			censoringTypeInterval.checked
		}
*/
		AssignedVariablesList
		{
			name:			 	"covariates"
			title:			 	qsTr("Covariates")
			allowedColumns:		["scale"]
		}

		AssignedVariablesList
		{
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["nominal"]
		}
	}

	Section
	{
		title: qsTr("Model")

		VariablesForm
		{
			preferredHeight:	jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name:	"availableTerms"
				title:	qsTr("Components")
				width:	parent.width / 4
				source:	['covariates', 'factors']
			}

			ModelTermsList
			{
				width:	parent.width * 5 / 9
			}
		}

		CheckBox
		{
			name:		"interceptTerm"
			label:		qsTr("Include intercept")
			checked:	true
		}
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			title:				qsTr("Coefficients")
			columns:			2
			Layout.columnSpan:	2

			Group
			{
				CheckBox
				{
					name:		"coefficientEstimate"
					label:		qsTr("Estimates")
					checked:	true

					CheckBox
					{
						name:	"vovkSellke"
						label:	qsTr("Vovk-Sellke maximum p-ratio")
					}
				}

				CheckBox
				{
					name:		"coefficientHazardRatioEstimates"
					label:		qsTr("Hazard ratio estimates")
					checked:	true

					CIField
					{
						name:	"coefficientCiLevel"
						label:	qsTr("Confidence intervals")
					}
				}
				
			}
		}
		
	}
}
