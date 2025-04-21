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
import QtQuick.Layouts
import JASP.Controls
import JASP

Form
{
	info: qsTr("This analysis performs a parametric survival analysis.")

	VariablesForm
	{
		removeInvisibles:	true
		preferredHeight:	((censoringTypeRight.checked  || censoringTypeLeft.checked) ? 400 : 450 ) * jaspTheme.uiScale

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
			visible:			censoringTypeInterval.checked
			property bool active:	censoringTypeInterval.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Interval.")
		}

		AssignedVariablesList
		{
			name:				"intervalEnd"
			title:				qsTr("Interval End")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeInterval.checked
			property bool active:	censoringTypeInterval.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Interval.")
		}

		AssignedVariablesList
		{
			name:				"timeToEvent"
			title:				qsTr("Time to Event")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeRight.checked || censoringTypeLeft.checked
			property bool active:	censoringTypeRight.checked || censoringTypeLeft.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
			info: qsTr("Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right or Left.")
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
			name:			 	"covariates"
			title:			 	qsTr("Covariates")
			allowedColumns:		["scale"]
			info: qsTr("Add continuous variables as covariates to include them in the Cox regression model.")
		}

		AssignedVariablesList
		{
			name:			 	"factors"
			title:			 	qsTr("Factors")
			allowedColumns:		["nominal"]
			info: qsTr("Add categorical variables as factors to include them in the Cox regression model.")
		}


		AssignedVariablesList
		{
			name:			 	"weights"
			title:			 	qsTr("Weights")
			allowedColumns:		["scale"]
			singleVariable:		true
			info: qsTr("Select a variable for case weights, weighting each observation accordingly in the model.")
		}
	}

	RadioButtonGroup
	{
		id:						censoringType
		Layout.columnSpan:		1
		name:					"censoringType"
		title:					qsTr("Censoring Type")
		radioButtonsOnSameRow:	true
		columns:				3
		info: qsTr("Select the type of censoring in your data: right, left, or interval censoring.")

		RadioButton
		{
			label:		qsTr("Right")
			value:		"right"
			id:			censoringTypeRight
			checked:	true
		}

		RadioButton
		{
			label:		qsTr("Interval")
			value:		"interval"
			id:			censoringTypeInterval
		}

		RadioButton
		{
			label:		qsTr("Left")
			value:		"left"
			id:			censoringTypeLeft
		}
	}

	DropDown
	{
		name:		"distribution"
		id:			distribution
		label:		qsTr("Distribution")
		info: qsTr("Choose the parametric distribution for the analysis. All fits and display results for all 'Selected parametric families' in the 'Advanced' section. 'Best AIC' and 'Best BIC' fit all `Selected parametric families` in the Advanced section and display the results only for a parametric family with the lowest AIC/BIC.")
		values:
		[
			{ label: qsTr("Generalized gamma"),					value: "generalizedGamma" },
			{ label: qsTr("Generalized F"),						value: "generalizedF" },
			{ label: qsTr("Weibull"),							value: "weibull" },
			{ label: qsTr("Gamma"),								value: "gamma" },
			{ label: qsTr("Exponential"),						value: "exponential" },
			{ label: qsTr("Log-logistic"),						value: "logLogistic" },
			{ label: qsTr("Log-normal"),						value: "logNormal" },
			{ label: qsTr("Gompertz"),							value: "gompertz" },
			{ label: qsTr("Generalized gamma (original)"),		value: "generalizedGammaOriginal" },
			{ label: qsTr("Generalized F (original)"),			value: "generalizedFOriginal" },
			{ label: qsTr("All"),								value: "all"},
			{ label: qsTr("Best AIC"),							value: "bestAIC"},
			{ label: qsTr("Best BIC"),							value: "bestBIC"}
		]
	}

	Section
	{
		title: qsTr("Strata, Cluster, and Frailty")

		VariablesForm
		{
			preferredHeight:		300

			AvailableVariablesList
			{
				name: "allVariablesList2"
			}

			AssignedVariablesList
			{
				name:			 	"strata"
				id:					strata
				title:			 	qsTr("Strata")
				allowedColumns:		["nominal"]
				info: qsTr("Select variables to define strata, allowing separate baseline hazard functions for each stratum.")
			}

			// TODO: allow only if multiple outcomes are possible
			/*
			AssignedVariablesList
			{
				name:			 	"id"
				title:			 	qsTr("Id")
				allowedColumns:		["nominal"]
				singleVariable:		true
			}
			*/

			AssignedVariablesList
			{
				name:			 	"cluster"
				id:					cluster
				enabled:			frailty.count == 0 && method.value != "exact"
				title:			 	qsTr("Cluster")
				allowedColumns:		["nominal"]
				singleVariable:		true
				info: qsTr("Select a variable to define clusters of correlated observations for robust variance estimation. Disabled when a Frailty variable is specified or Method is set to Exact.")
			}

			AssignedVariablesList
			{
				name:			 	"frailty"
				id:					frailty
				enabled:			cluster.count == 0
				title:			 	qsTr("Frailty")
				allowedColumns:		["nominal"]
				singleVariable:		true
				info: qsTr("Select a variable for frailty to model unobserved heterogeneity using random effects. Disabled when a Cluster variable is specified.")
			}
		}

		Group
		{
			title:		qsTr("Frailty")
			enabled:	frailty.count > 0

			DropDown
			{
				name:		"frailtyDistribution"
				id:			frailtyDistribution
				label:		qsTr("Distribution")
				info: qsTr("Choose the distribution for the frailty term: Gamma, Gaussian, or T distribution. Only available when a Frailty variable is specified.")
				values:
				[
					{ label: qsTr("Gamma"),		value: "gamma"},
					{ label: qsTr("Gaussian"),	value: "gaussian"},
					{ label: qsTr("T"),			value: "t"}
				]
			}

			DropDown
			{
				name:		"frailtyMethod"
				id:			frailtyMethod
				label:		qsTr("Method")
				info: qsTr("Select the estimation method for the frailty distribution. Options vary based on the chosen Distribution.")
				values:		(function() {
					if (frailtyDistribution.value == "gamma") {
						return [
							{ label: qsTr("EM"),		value: "em"},
							{ label: qsTr("AIC"),		value: "aic"},
							{ label: qsTr("Fixed"),		value: "fixed"}
						];
					} else if (frailtyDistribution.value == "gaussian") {
						return [
							{ label: qsTr("REML"),		value: "reml"},
							{ label: qsTr("AIC"),		value: "aic"},
							{ label: qsTr("Fixed"),		value: "fixed"}
						];
					} else if (frailtyDistribution.value == "t") {
						return [
							{ label: qsTr("AIC"),		value: "aic"},
							{ label: qsTr("Fixed"),		value: "fixed"}
						];
					}
				})()
			}

			DoubleField
			{
				label:			qsTr("Df")
				visible:		frailtyDistribution.value == "t"
				name:			"frailtyMethodTDf"
				defaultValue:	5
				info: qsTr("Set the degrees of freedom (Df) for the T frailty distribution. Only visible when Distribution is set to T.")
			}

			Group
			{
				visible:	frailtyMethod.value == "fixed"

				DropDown
				{
					name:		"frailtyMethodFixed"
					id:			frailtyMethodFixed
					label:		qsTr("Fix")
					info: qsTr("Choose the parameter to fix in the frailty model when Method is set to Fixed: Theta or Df.")
					values:
					[
						{ label: qsTr("Theta"),	value: "theta"},
						{ label: qsTr("Df"),	value: "df"}
					]
				}

				DoubleField
				{
					label:			qsTr("Theta")
					visible:		frailtyMethodFixed.value == "theta"
					name:			"frailtyMethodFixedTheta"
					defaultValue:	0
					info: qsTr("Specify the value of Theta to fix in the frailty model. Only visible when Fix is set to Theta.")
				}

				DoubleField
				{
					label:			qsTr("Df")
					visible:		frailtyMethodFixed.value == "df"
					name:			"frailtyMethodFixedDf"
					defaultValue:	0
					info: qsTr("Specify the degrees of freedom (Df) to fix in the frailty model. Only visible when Fix is set to Df.")
				}
			}
		}
	}


	Section
	{
		title: qsTr("Advanced")

		Group
		{
			title:		qsTr("Selected parametric families")
			enabled:	distribution.value == "all" || distribution.value == "bestAIC" || distribution.value == "bestBIC"

			CheckBox { name: "SelectedParametricFamilyGeneralizedGamma";			label: qsTr("Generalized gamma");				checked: true}
			CheckBox { name: "SelectedParametricFamilyGeneralizedF";				label: qsTr("Generalized F");					checked: true }
			CheckBox { name: "SelectedParametricFamilyWeibull";						label: qsTr("Weibull");							checked: true }
			CheckBox { name: "SelectedParametricFamilyGamma";						label: qsTr("Gamma");							checked: true }
			CheckBox { name: "SelectedParametricFamilyExponential";					label: qsTr("Exponential");						checked: true }
			CheckBox { name: "SelectedParametricFamilyLogLogistic";					label: qsTr("Log-logistic");					checked: true }
			CheckBox { name: "SelectedParametricFamilyLogNormal";					label: qsTr("Log-normal");						checked: true }
			CheckBox { name: "SelectedParametricFamilyGompertz";					label: qsTr("Gompertz");						checked: true }
			CheckBox { name: "SelectedParametricFamilyGeneralizedGammaOriginal"; 	label: qsTr("Generalized gamma (original)");	checked: false }
			CheckBox { name: "SelectedParametricFamilyGeneralizedFOriginal"; 		label: qsTr("Generalized F (original)");		checked: false }
		}
	}
}
