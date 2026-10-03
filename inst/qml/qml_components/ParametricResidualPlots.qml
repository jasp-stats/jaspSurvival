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

Group
{
	id:	residualsRoot
	property bool rightCensoring:	false
	property string residualVsPredictedLabel:	qsTr("Residuals vs. predicted survival")

	title: qsTr("Residual Plots")

	CheckBox
	{
		name:		"residualPlotResidualVsTime"
		label:		qsTr("Residuals vs. time")
		enabled:	residualsRoot.rightCensoring
		info: qsTr("Plot residuals versus time. Only available when Censoring Type is set to Right.")
	}

	CheckBox
	{
		name:		"residualPlotResidualVsPredictors"
		label:		qsTr("Residuals vs. predictors")
		info: qsTr("Plot residuals versus predictors to assess model fit. Available when model terms are specified.")
	}

	CheckBox
	{
		name:		"residualPlotResidualVsPredicted"
		label:		residualsRoot.residualVsPredictedLabel
		info: qsTr("Plot residuals versus predicted mean survival times.")
	}

	CheckBox
	{
		name:		"residualPlotResidualHistogram"
		label:		qsTr("Residuals histogram")
		info: qsTr("Display a histogram of residuals to assess their distribution.")
	}

	DropDown
	{
		name:		"residualPlotResidualType"
		id:			residualPlotResidualType
		label:		qsTr("Type")
		info: qsTr("Select the type of residuals to plot")
		values:		[
			{ label: qsTr("Response"),				value: "response"},
			{ label: qsTr("Cox-Snell"),				value: "coxSnell"}
		]
	}
}
