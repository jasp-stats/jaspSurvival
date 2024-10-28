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
	info: qsTr("This analysis performs a Cox proportional hazards regression, a statistical method used in survival analysis to examine the relationship between the time until an event occurs (such as death or failure) and one or more predictor variables. It helps you understand how different factors influence the risk or hazard of the event happening at any given time. The Cox model is particularly useful because it can handle censored data (when the event has not occurred for some subjects during the study period) and does not require assumptions about the baseline hazard function.")

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
			info: qsTr("Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Counting.")
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
			info: qsTr("Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Counting.")
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
			info: qsTr("Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right.")
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
		info: qsTr("Choose the method for handling tied event times: Efron, Breslow, or Exact.")
	}

	RadioButtonGroup
	{
		id:						censoringType
		Layout.columnSpan:		1
		name:					"censoringType"
		title:					qsTr("Censoring Type")
		radioButtonsOnSameRow:	true
		columns:				2
		info: qsTr("Select the type of censoring in your data: Right censoring or Counting process.")

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
				info: qsTr("Include overall model fit statistics in the output.")
			}

			Group
			{
				title:	qsTr("Tests")
				info: qsTr("Test all parameters of the H₁ model.")

				CheckBox
				{
					name:		"testsLikelihoodRatio"
					label:		qsTr("Likelihood ratio")
					info: qsTr("Include the Likelihood Ratio Test in the model summary.")
				}

				CheckBox
				{
					name:		"testsWald"
					enabled:	frailty.count == 0
					label:		qsTr("Wald")
					info: qsTr("Include the Wald Test in the model summary. Disabled when a Frailty variable is specified.")
				}

				CheckBox
				{
					name:		"testsScore"
					enabled:	frailty.count == 0
					label:		qsTr("Score (log-rank)")
					info: qsTr("Include the Score (Log-Rank) Test in the model summary. Disabled when a Frailty variable is specified.")
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
					info: qsTr("Display the estimated coefficients in the model output.")

					CheckBox
					{
						name:	"vovkSellke"
						label:	qsTr("Vovk-Sellke maximum p-ratio")
						info: qsTr("Include the Vovk-Sellke maximum p-ratio for each coefficient.")
					}
				}

				CheckBox
				{
					name:		"coefficientHazardRatioEstimates"
					label:		qsTr("Hazard ratio estimates")
					checked:	true
					info: qsTr("Display the hazard ratio estimates (exponentiated coefficients).")

					CheckBox
					{
						name:		"coefficientHazardRatioEstimatesIncludeFrailty"
						label:		qsTr("Include frailty")
						enabled:	frailty.count > 0
						info: qsTr("Include the effect of frailty in hazard ratio estimates. Only available when a Frailty variable is specified.")
					}
				}

				CheckBox
				{
					name:				"coefficientsConfidenceIntervals"
					label:				qsTr("Confidence intervals")
					checked:			true
					childrenOnSameRow:	true
					info: qsTr("Include confidence intervals for the coefficients.")

					CIField
					{
						name:	"coefficientsConfidenceIntervalsLevel"
						info: qsTr("Set the confidence level (e.g., 95%) for the confidence intervals.")
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
				info: qsTr("Provide a table to test the proportional hazards assumption.")
			}

			CheckBox
			{
				name:		"proportionalHazardsPlot"
				label:		qsTr("Plot")
				info: qsTr("Generate plots to assess the proportional hazards assumption.")
			}

			DropDown
			{
				name:		"proportionalHazardsTransformation"
				label:		qsTr("Transformation")
				info: qsTr("Select the transformation for time in proportional hazards testing: KM, Rank, or Identity.")
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
				info: qsTr("Test the proportional hazards assumption for individual model terms (instead of coefficients).")
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
				info: qsTr("Plot residuals versus time to detect non-proportional hazards. Available only for Martingale or Deviance residuals.")
			}

			CheckBox
			{
				name:		"residualPlotResidualVsPredictors"
				label:		qsTr("Residuals vs. predictors")
				enabled:	selectedModelTerms.count > 0
				info: qsTr("Plot residuals versus predictors to assess model fit. Available when model terms are specified.")
			}

			CheckBox
			{
				name:		"residualPlotResidualVsPredicted"
				label:		qsTr("Residuals vs. predicted survival")
				enabled:	residualPlotResidualType.value == "martingale" || residualPlotResidualType.value == "deviance"
				info: qsTr("Plot residuals versus predicted survival probabilities. Available only for Martingale or Deviance residuals.")
			}

			CheckBox
			{
				name:		"residualPlotResidualHistogram"
				label:		qsTr("Residuals histogram")
				enabled:	residualPlotResidualType.value == "martingale" || residualPlotResidualType.value == "deviance"
				info: qsTr("Display a histogram of residuals to assess their distribution. Available only for Martingale or Deviance residuals.")
			}

			DropDown
			{
				name:		"residualPlotResidualType"
				id:			residualPlotResidualType
				label:		qsTr("Type")
				info: qsTr("Select the type of residuals to plot: Martingale, Deviance, Score, Schoenfeld, or Scaled Schoenfeld.")
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
