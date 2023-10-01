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
			suggestedColumns:	["scale"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:				"eventStatus"
			title:				qsTr("Event Status")
			suggestedColumns:	["nominal"]
			singleVariable:		true
		}

		DropDown
		{
			name:				"eventIndicator"
			label:				qsTr("Event Indicator")
			source:				[{name: "eventStatus", use: "levels"}]
		}

		AssignedVariablesList
		{
			name:			 	"covariates"
			title:			 	qsTr("Covariates")
			allowedColumns:		["ordinal", "scale"]
		}

		AssignedVariablesList
		{
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["ordinal", "nominal", "nominalText"]
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
