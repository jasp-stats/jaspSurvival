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

Group
{
	id: distributions
	property var constraintControl: null
	property bool constraintActive: false
	property string selectedDistribution: "weibull"

	title:		qsTr("Selected Parametric Distributions")
	enabled:	selectedDistribution === "all" || selectedDistribution === "bestAic" || selectedDistribution === "bestBic"

	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionExponential"
		label: qsTr("Exponential")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	CheckBox { name: "selectedParametricDistributionGamma";						label: qsTr("Gamma");							checked: true }
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionGeneralizedF"
		label: qsTr("Generalized F")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionGeneralizedGamma"
		label: qsTr("Generalized gamma")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionGompertz"
		label: qsTr("Gompertz")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	CheckBox { name: "selectedParametricDistributionLogLogistic";				label: qsTr("Log-logistic");					checked: true }
	CheckBox { name: "selectedParametricDistributionLogNormal";					label: qsTr("Log-normal");						checked: true }
	CheckBox { name: "selectedParametricDistributionWeibull";					label: qsTr("Weibull");							checked: true }
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionGeneralizedGammaOriginal"
		label: qsTr("Generalized gamma (original)")
		checked: false
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionGeneralizedFOriginal"
		label: qsTr("Generalized F (original)")
		checked: false
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
}
