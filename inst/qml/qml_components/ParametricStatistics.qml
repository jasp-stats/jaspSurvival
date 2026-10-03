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
// Preserve translation keys used by the published parametric form.
pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	id:	statisticsRoot
	property bool multipleResults:	false
	property bool multipleModels:	false
	property string modelSummaryInfo:	qsTr("Include a table with information about the model fit.")
	property alias extraStatisticsControls:	statisticsExtras.content

	title: qsTr("Statistics")

	CheckBox
	{
		label:		qsTr("Model summary")
		name:		"modelSummary"
		checked:	true
		info: statisticsRoot.modelSummaryInfo

		CheckBox
		{
			name:		"modelSummaryRankModels"
			label:		qsTr("Rank models")
			enabled:	statisticsRoot.multipleResults
			info: qsTr("Rank models based on the selected criterion.")

			RadioButtonGroup
			{
				name:		"modelSummaryRankModelsBy"

				RadioButton
				{
					label:		qsTr("AIC")
					value:		"aic"
					checked:	true
				}

				RadioButton
				{
					label:		qsTr("BIC")
					value:		"bic"
				}

				RadioButton
				{
					label:		qsTr("Log lik.")
					value:		"logLik"
				}
			}
		}

		CheckBox
		{
			name:		"modelSummaryAicWeighs"
			label:		qsTr("AIC weights")
			enabled:	statisticsRoot.multipleResults
			info: qsTr("Include AIC weights in the model summary.")
		}

		CheckBox
		{
			name:		"modelSummaryBicWeighs"
			label:		qsTr("BIC weights")
			enabled:	statisticsRoot.multipleResults
			info: qsTr("Include BIC weights in the model summary.")
		}
	}

	Group
	{
		CheckBox
		{
			label:		qsTr("Coefficients")
			name:		"coefficients"
			checked:	false
			info: qsTr("Include a table with coefficient estimates.")

			CheckBox
			{
				name:				"coefficientsConfidenceInterval"
				label:				qsTr("Confidence intervals")
				checked:			true
				childrenOnSameRow:	true
				info: qsTr("Include confidence intervals for the coefficients.")

				CIField
				{
					name: "coefficientsConfidenceIntervalLevel"
					info: qsTr("Set the confidence level for the confidence intervals.")
				}
			}
		}

		CheckBox
		{
			label:		qsTr("Coefficients covariance matrix")
			name:		"coefficientsCovarianceMatrix"
			checked:	false
			info: qsTr("Include a table with the covariance matrix of the coefficient estimates.")
		}

		Group
		{
			id:		statisticsExtras
			visible:	hasChildren
		}
	}

	CheckBox
	{
		label:		qsTr("Sequential model comparison")
		name:		"sequentialModelComparison"
		checked:	false
		enabled:	statisticsRoot.multipleModels
		info: qsTr("Include a table with the results of the sequential model comparison.")
	}

}
