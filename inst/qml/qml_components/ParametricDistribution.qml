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
	id: distributionGroup
	property bool mixture: false
	property bool constraintActive: false
	property var selectedFamilies: []
	property int componentCount: 1
	readonly property alias value: distribution.value
	readonly property alias currentLabel: distribution.currentLabel
	readonly property alias parametersRestricted: restrictParameters.checked
	readonly property var familyChoices:
	[
		{ label: qsTr("Exponential"), value: "exponential" },
		{ label: qsTr("Gamma"), value: "gamma" },
		{ label: qsTr("Generalized F"), value: "generalizedF" },
		{ label: qsTr("Generalized gamma"), value: "generalizedGamma" },
		{ label: qsTr("Gompertz"), value: "gompertz" },
		{ label: qsTr("Log-logistic"), value: "logLogistic" },
		{ label: qsTr("Log-normal"), value: "logNormal" },
		{ label: qsTr("Weibull"), value: "weibull" },
		{ label: qsTr("Generalized gamma (original)"), value: "generalizedGammaOriginal" },
		{ label: qsTr("Generalized F (original)"), value: "generalizedFOriginal" }
	]
	columns: 1

	DropDown
	{
		depends: distributionGroup.depends
		name:		"distribution"
		id:			distribution
		label:		qsTr("Distribution")
		startValue:	"weibull"
		info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Choose the distribution shared by all mixture components. All, Best AIC and Best BIC compare the distributions selected in Advanced.") : qsTr("Choose a distribution, show all selected distributions, or select the best using AIC or BIC.")
		values: distributionGroup.familyChoices.concat([
			{ label: qsTr("All"),								value: "all"},
			{ label: qsTr("Best AIC"),							value: "bestAic"},
			{ label: qsTr("Best BIC"),							value: "bestBic"}
		]).filter(function(item) {
			return !distributionGroup.constraintActive || ["gamma", "logLogistic", "logNormal", "weibull", "all", "bestAic", "bestBic"].indexOf(item.value) >= 0
		})
	}

	CheckBox
	{
		id: restrictParameters
		name: "restrictParameters"
		label: qsTr("Restrict parameters")
		info: qsTr("Fix baseline distribution parameters at specified values. Leave a field empty to estimate the parameter.")
	}
}
