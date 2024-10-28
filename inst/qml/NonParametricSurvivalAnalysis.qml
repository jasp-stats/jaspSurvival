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
	info: qsTr("This analysis computes a survival curve for censored data using the Kaplan-Meier estimator for single-event survival data. It estimates the probability of survival over time, allowing you to understand and visualize the distribution of survival times within your data. The analysis accommodates censored observations (subjects for whom the event has not occurred during the study period) and enables comparison of survival curves across different groups using statistical tests such as the Log-Rank test, Peto and Peto test, and Fleming-Harrington test. Additionally, you can generate life tables to summarize survival data at specified intervals.")

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
			info: qsTr("Select the variable that represents the time until the event or censoring occurs.")
		}

		AssignedVariablesList
		{
			id:					eventStatusId
			name:				"eventStatus"
			title:				qsTr("Event Status")
			allowedColumns:		["nominal"]
			singleVariable:		true
			info: qsTr("Choose the variable that indicates the event status, specifying whether each observation is an event or censored.")
		}

		DropDown
		{
			name:				"eventIndicator"
			label:				qsTr("Event Indicator")
			source:				[{name: "eventStatus", use: "levels"}]
			onCountChanged:		currentIndex = 1
			info: qsTr("Specify the value in the Event Status variable that indicates the occurrence of the event.")
		}

		AssignedVariablesList
		{
			name:			 	"strata"
			id:					strata
			title:			 	qsTr("Strata")
			allowedColumns:		["nominal"]
			info: qsTr("Select variables to define strata, allowing separate survival curves for each stratum.")
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
				info: qsTr("Include the Log-Rank (Mantel-Haenszel) test to compare survival curves across strata.")
			}

			CheckBox
			{
				name:		"testsPetoAndPeto"
				label:		qsTr("Peto and Peto")
				info: qsTr("Include the Peto and Peto modification of the Gehan-Wilcoxon test to compare survival curves across strata.")
			}

			CheckBox
			{
				name:		"testsFlemmingHarrington"
				label:		qsTr("Flemming-Harrington")
				info: qsTr("Include the Fleming-Harrington test to compare survival curves across strata., with a customizable rho parameter.")

				DoubleField
				{
					name:			"testsFlemmingHarringtonRho"
					label:			qsTr("Rho")
					defaultValue:	0.5
					min:			0
					max:			1
					info: qsTr("Set the rho parameter for the Fleming-Harrington test, controlling the weight given to different time points (values between 0 and 1).")
				}
			}
		}

		CheckBox
		{
			name:	"lifeTable"
			label:	qsTr("Life table")
			info: qsTr("Generate a life table summarizing survival data at specified time intervals.")

			DropDown
			{
				name:		"lifeTableStepsType"
				id:			lifeTableStepsType
				label:		qsTr("Steps type")
				info: qsTr("Select the method to define intervals for the life table: Default, Quantiles, or Fixed size.")
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
				info: qsTr("Specify the number of intervals when using Quantiles as the steps type.")
			}

			CheckBox
			{
				name:		"lifeTableRoundSteps"
				label:		qsTr("Round steps")
				checked:	true
				visible:	lifeTableStepsType.value == "quantiles"
				info: qsTr("Round the interval boundaries to the nearest integer when using Quantiles steps.")
			}

			DoubleField
			{
				name:			"lifeTableStepsFrom"
				id:				lifeTableStepsFrom
				label:			qsTr("From")
				defaultValue:	0
				max:			lifeTableStepsTo.value
				visible:		lifeTableStepsType.value == "fixedSize"
				info: qsTr("Set the starting time for intervals when using Fixed size steps.")
			}

			DoubleField
			{
				name:			"lifeTableStepsSize"
				label:			qsTr("Size")
				defaultValue:	1
				// max:			lifeTableStepsTo.value // TODO: enable once max is data dependent
				visible:		lifeTableStepsType.value == "fixedSize"
				info: qsTr("Define the size of each interval when using Fixed size steps.")
			}

			DoubleField
			{
				name:			"lifeTableStepsTo"
				id:				lifeTableStepsTo
				label:			qsTr("To")
				defaultValue:	0
				min:			lifeTableStepsFrom.value
				visible:		lifeTableStepsType.value == "fixedSize"
				info: qsTr("Set the ending time for intervals when using Fixed size steps.")
			}
		}
	}

	SA.SurvivalPlot{}
}
