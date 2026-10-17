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

DropDown
{
	property bool mixture: false
	property bool constraintActive: false

	name:		"distribution"
	id:			distribution
	label:		qsTr("Distribution")
	startValue:	"weibull"
	info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Choose the parametric distribution of the mixture components (all components come from the same distribution). All fits and display results for all 'Selected parametric families' in the 'Advanced' section. 'Best AIC' and 'Best BIC' fit all `Selected parametric families` in the Advanced section and display the results only for a parametric family with the lowest AIC/BIC. Families without a closed-form weighted fit (gamma, Gompertz, and the generalized families) are considerably slower to estimate.") : qsTr("Choose the parametric distribution for the analysis. All fits and display results for all 'Selected parametric families' in the 'Advanced' section. 'Best AIC' and 'Best BIC' fit all `Selected parametric families` in the Advanced section and display the results only for a parametric family with the lowest AIC/BIC.")
	values:
	[
		{ label: qsTr("Exponential"),						value: "exponential" },
		{ label: qsTr("Gamma"),								value: "gamma" },
		{ label: qsTr("Generalized F"),						value: "generalizedF" },
		{ label: qsTr("Generalized gamma"),					value: "generalizedGamma" },
		{ label: qsTr("Gompertz"),							value: "gompertz" },
		{ label: qsTr("Log-logistic"),						value: "logLogistic" },
		{ label: qsTr("Log-normal"),						value: "logNormal" },
		{ label: qsTr("Weibull"),							value: "weibull" },
		{ label: qsTr("Generalized gamma (original)"),		value: "generalizedGammaOriginal" },
		{ label: qsTr("Generalized F (original)"),			value: "generalizedFOriginal" },
		{ label: qsTr("All"),								value: "all"},
		{ label: qsTr("Best AIC"),							value: "bestAic"},
		{ label: qsTr("Best BIC"),							value: "bestBic"}
	].filter(function(item) {
		return !constraintActive || ["gamma", "logLogistic", "logNormal", "weibull", "all", "bestAic", "bestBic"].indexOf(item.value) >= 0
	})
}
