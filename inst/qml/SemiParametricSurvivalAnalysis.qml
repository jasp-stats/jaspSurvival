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
		preferredHeight:	(censoringTypeRight.checked ? 400 : 450 ) * jaspTheme.uiScale

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
			property bool active:	censoringTypeCounting.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"intervalEnd"
			title:				qsTr("Interval End")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeCounting.checked
			property bool active:	censoringTypeCounting.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"timeToEvent"
			title:				qsTr("Time to Event")
			allowedColumns:		["scale"]
			singleVariable:		true
			visible:			censoringTypeRight.checked	
			property bool active:	censoringTypeRight.checked
			onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
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
			name:			 	"weights"
			title:			 	qsTr("Weights")
			allowedColumns:		["scale"]
			singleVariable:		true
		}
	}

	DropDown
	{
		name:		"method"
		id:			method
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
		title: qsTr("Strata, Cluster, and Frailty")

		VariablesForm
		{
			// TODO Bruno: the heigh adjustment does not seem to work
			height:		300

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
			}

			AssignedVariablesList
			{
				name:			 	"frailty"
				id:					frailty
				enabled:			cluster.count == 0
				title:			 	qsTr("Frailty")
				allowedColumns:		["nominal"]
				singleVariable:		true
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
			}

			Group
			{
				visible:	frailtyMethod.value == "fixed"

				DropDown
				{
					name:		"frailtyMethodFixed"
					id:			frailtyMethodFixed
					label:		qsTr("Fix")
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
				}

				DoubleField
				{
					label:			qsTr("Df")
					visible:		frailtyMethodFixed.value == "df"
					name:			"frailtyMethodFixedDf"
					defaultValue:	0
				}
			}
		}
	}

	Section
	{
		title:	qsTr("Model")

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
				id:		selectedModelTerms
			}
		}
	}

	Section
	{
		title:		qsTr("Statistics")

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
					enabled:	frailty.count == 0
					label:		qsTr("Wald")
				}

				CheckBox
				{
					name:		"testsScore"
					enabled:	frailty.count == 0
					label:		qsTr("Score (log-rank)")
				}
			}		
		}

		Group
		{
			title:				qsTr("Coefficients")

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

					CheckBox
					{
						name:		"coefficientHazardRatioEstimatesIncludeFrailty"
						label:		qsTr("Include frailty")
						enabled:	frailty.count > 0
					}
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

	Section
	{
		title:	qsTr("Diagnostics")

		Group
		{
			title:	qsTr("Proportional Hazards")

			CheckBox
			{
				name:		"proportionalHazardsTable"
				label:		qsTr("Table")
			}

			CheckBox
			{
				name:		"proportionalHazardsPlot"
				label:		qsTr("Plot")
			}

			DropDown
			{
				name:		"proportionalHazardsTransformation"
				label:		qsTr("Transformation")
				values:
				[
					{ label: qsTr("KM"),			value: "km"},
					{ label: qsTr("Rank"),			value: "rank"},
					{ label: qsTr("Identity"),		value: "identity"}
				]
			}

			CheckBox
			{
				name:		"proportionalHazardsTestTerms"
				label:		qsTr("Test terms")
			}			

		}

		Group
		{
			title: qsTr("Residuals Plots")
			
			CheckBox
			{
				name:		"residualPlotResidualVsTime"
				label:		qsTr("Residuals vs. time")
				enabled:	 residualPlotResidualType.value == "martingale" || residualPlotResidualType.value == "deviance"
			}

			CheckBox
			{
				name:		"residualPlotResidualVsPredictors"
				label:		qsTr("Residuals vs. predictors")
				enabled:	selectedModelTerms.count > 0
			}

			CheckBox
			{
				name:		"residualPlotResidualVsPredicted"
				label:		qsTr("Residuals vs. predicted survival")
				enabled:	residualPlotResidualType.value == "martingale" || residualPlotResidualType.value == "deviance"
			}

			CheckBox
			{
				name:		"residualPlotResidualHistogram"
				label:		qsTr("Residuals histogram")
				enabled:	residualPlotResidualType.value == "martingale" || residualPlotResidualType.value == "deviance"
			}

			DropDown
			{
				name:		"residualPlotResidualType"
				id:			residualPlotResidualType
				label:		qsTr("Type")
				values:		[
					{ label: qsTr("Martingale"),			value: "martingale"},
					{ label: qsTr("Deviance"),				value: "deviance"},
					{ label: qsTr("Score"),					value: "score"},
					{ label: qsTr("Schoenfeld"),			value: "schoenfeld"},
					{ label: qsTr("Scaled Schoenfeld"),		value: "scaledSchoenfeld"}
				]
			}
		}
	}
}
