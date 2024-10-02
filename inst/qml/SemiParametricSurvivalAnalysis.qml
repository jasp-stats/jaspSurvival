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

import "./qml_components"		as SA

Form
{
	VariablesForm
	{
		removeInvisibles:	true
		height:		1000

		AvailableVariablesList
		{
			name: "allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"intervalStart"
			title:				qsTr("Interval Start")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeCounting.checked
		}

		AssignedVariablesList
		{
			name:				"intervalEnd"
			title:				qsTr("Interval End")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeCounting.checked
		}

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

		AssignedVariablesList
		{
			name:			 	"strata"
			title:			 	qsTr("Strata")
			allowedColumns:		["nominal"]
		}

		AssignedVariablesList
		{
			name:			 	"id"
			title:			 	qsTr("Id")
			allowedColumns:		["nominal"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:			 	"cluster"
			title:			 	qsTr("Cluster")
			allowedColumns:		["nominal"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:			 	"weights"
			title:			 	qsTr("Weights")
			allowedColumns:		["scale"]
			singleVariable:		true
		}
	}

	DropDown
	{
		name:		"method"
		label:		qsTr("Method")
		values:
		[
			{ label: qsTr("Efron"),			value: "efron"},
			{ label: qsTr("Breslow"),		value: "breslow"},
			{ label: qsTr("Exact"),			value: "exact"}
		]
	}

	RadioButtonGroup
	{
		id:						censoringType
		Layout.columnSpan:		1
		name:					"censoringType"
		title:					qsTr("Censoring Type")
		radioButtonsOnSameRow:	true
		columns:				2

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
				source:	['covariates', 'factors', 'strata'] 
			}

			ModelTermsList
			{
				width:	parent.width * 5 / 9
			}
		}
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{

			CheckBox
			{
				name:		"modelFit"
				label:		qsTr("Model fit")
			}

			Group
			{
				title:	qsTr("Tests")

				CheckBox
				{
					name:		"testsLikelihoodRatio"
					label:		qsTr("Likelihood ratio")
				}

				CheckBox
				{
					name:		"testsWald"
					label:		qsTr("Wald")
				}

				CheckBox
				{
					name:		"testsScore"
					label:		qsTr("Score (log-rank)")
				}
			}		
		}

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
				}

				CheckBox
				{
					name:				"coefficientsConfidenceIntervals"
					label:				qsTr("Confidence intervals")
					checked:			true
					childrenOnSameRow:	true
					
					CIField
					{
						name:	"coefficientsConfidenceIntervalsLevel"
					}
				}				
			}
		}
	}

	Section
	{
		title:	qsTr("Plot")

		SA.SurvivalPlot{}
	}
}
