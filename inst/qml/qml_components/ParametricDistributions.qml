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
	readonly property var selectedFamilies: [
		{label: qsTr("Exponential"), value: "exponential", selected: exponential.checked},
		{label: qsTr("Gamma"), value: "gamma", selected: gamma.checked},
		{label: qsTr("Generalized F"), value: "generalizedF", selected: generalizedF.checked},
		{label: qsTr("Generalized gamma"), value: "generalizedGamma", selected: generalizedGamma.checked},
		{label: qsTr("Gompertz"), value: "gompertz", selected: gompertz.checked},
		{label: qsTr("Log-logistic"), value: "logLogistic", selected: logLogistic.checked},
		{label: qsTr("Log-normal"), value: "logNormal", selected: logNormal.checked},
		{label: qsTr("Weibull"), value: "weibull", selected: weibull.checked},
		{label: qsTr("Generalized gamma (original)"), value: "generalizedGammaOriginal", selected: generalizedGammaOriginal.checked},
		{label: qsTr("Generalized F (original)"), value: "generalizedFOriginal", selected: generalizedFOriginal.checked}
	].filter(function(item) { return item.selected; })

	title:		qsTr("Selected Parametric Distributions")
	enabled:	selectedDistribution === "all" || selectedDistribution === "bestAic" || selectedDistribution === "bestBic"

	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionsExponential"
		id: exponential
		label: qsTr("Exponential")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	CheckBox { name: "selectedParametricDistributionsGamma"; id: gamma;			label: qsTr("Gamma");							checked: true }
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionsGeneralizedF"
		id: generalizedF
		label: qsTr("Generalized F")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionsGeneralizedGamma"
		id: generalizedGamma
		label: qsTr("Generalized gamma")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionsGompertz"
		id: gompertz
		label: qsTr("Gompertz")
		checked: true
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	CheckBox { name: "selectedParametricDistributionsLogLogistic"; id: logLogistic;	label: qsTr("Log-logistic");					checked: true }
	CheckBox { name: "selectedParametricDistributionsLogNormal"; id: logNormal;	label: qsTr("Log-normal");						checked: true }
	CheckBox { name: "selectedParametricDistributionsWeibull"; id: weibull;		label: qsTr("Weibull");							checked: true }
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionsGeneralizedGammaOriginal"
		id: generalizedGammaOriginal
		label: qsTr("Generalized gamma (original)")
		checked: false
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
	ConstraintFamilyCheckBox
	{
		name: "selectedParametricDistributionsGeneralizedFOriginal"
		id: generalizedFOriginal
		label: qsTr("Generalized F (original)")
		checked: false
		depends: distributions.constraintControl
		constraintActive: distributions.constraintActive
	}
}
