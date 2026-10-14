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

VariablesForm
{
	id: variables
	property bool mixture: false
	property bool rightCensoring: true
	property bool intervalCensoring: false
	property bool countingCensoring: false
	readonly property int factorCount: factors.count
	readonly property int subgroupCount: subgroup.count

	removeInvisibles:	true
	preferredHeight:	((rightCensoring  || intervalCensoring) ? 475 : 550 ) * jaspTheme.uiScale

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
		visible:			intervalCensoring || countingCensoring
		property bool active:	intervalCensoring  || countingCensoring
		onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
		info: qsTr("Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Interval or Counting.")
	}

	AssignedVariablesList
	{
		name:				"intervalEnd"
		title:				qsTr("Interval End")
		allowedColumns:		["scale"]
		singleVariable:		true
		visible:			intervalCensoring || countingCensoring
		property bool active:	intervalCensoring || countingCensoring
		onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
		info: qsTr("Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Interval or Counting.")
	}

	AssignedVariablesList
	{
		name:				"timeToEvent"
		title:				qsTr("Time to Event")
		allowedColumns:		["scale"]
		singleVariable:		true
		visible:			rightCensoring
		property bool active:	rightCensoring
		onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
		info: qsTr("Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right.")
	}

	AssignedVariablesList
	{
		id:					eventStatusId
		name:				"eventStatus"
		title:				qsTr("Event Status")
		visible:			rightCensoring || countingCensoring
		property bool active:	rightCensoring || countingCensoring
		allowedColumns:		["nominal"]
		singleVariable:		true
		info: qsTr("Choose the variable that indicates the event status, specifying whether each observation is an event or censored.")
	}

	DropDown
	{
		name:				"eventIndicator"
		label:				qsTr("Event Indicator")
		visible:			rightCensoring || countingCensoring
		property bool active:	rightCensoring || countingCensoring
		source:				[{name: "eventStatus", use: "levels"}]
		onCountChanged:		currentIndex = 1
		info: qsTr("Specify the value in the Event Status variable that indicates the occurrence of the event.")
	}

	AssignedVariablesList
	{
		id:				 	covariates
		name:			 	"covariates"
		title:			 	qsTr("Covariates")
		allowedColumns:		["scale"]
		info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Add continuous variables as covariates to include them in the mixture model. The covariates affect the location parameter of each component with separate coefficients.") : qsTr("Add continuous variables as covariates to include them in the parametric survival model.")
	}

	AssignedVariablesList
	{
		id:				 	factors
		name:			 	"factors"
		title:			 	qsTr("Factors")
		allowedColumns:		["nominal"]
		info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Add categorical variables as factors to include them in the mixture model. The factors affect the location parameter of each component with separate coefficients.") : qsTr("Add categorical variables as factors to include them in the parametric survival model.")
	}


	AssignedVariablesList
	{
		name:			 	"weights"
		title:			 	qsTr("Weights")
		allowedColumns:		["scale"]
		singleVariable:		true
		info: qsTr("Select a variable for case weights, weighting each observation accordingly in the model.")
	}

	AssignedVariablesList
	{
		name:			 	"subgroup"
		id:					subgroup
		title:			 	qsTr("Subgroup")
		height:				1.5 * jaspTheme.defaultSingleItemListHeight
		allowedColumns:		["nominal"]
		info: qsTr("Select one or more factors for subgroup analysis. Separate models are fitted for each observed combination of their levels.")
	}
}
