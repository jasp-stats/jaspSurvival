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
	property bool mixture: false
	property bool hasSubgroup: false
	property bool multipleModels: false
	property alias extraControls: extras.content
	property string selectedDistribution: "weibull"

	title:		qsTr("Output Formatting")

	CheckBox
	{
		name:		"includeFullDatasetInSubgroupAnalysis"
		text:		qsTr("Include full dataset in subgroup analysis")
		enabled:	hasSubgroup
		checked:	false
		info: qsTr("Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.")
	}

	CheckBox
	{
		name:		"compareModelsAcrossDistributions"
		text:		qsTr("Compare models across distributions")
		enabled:	(selectedDistribution === "all" || selectedDistribution === "bestAic" || selectedDistribution === "bestBic") && multipleModels
		checked:	true
		info: qsTr("Compare models across distributions. This option is only available when the multiple models and parametric distributions are specified.")
	}

	Group
	{
		id: extras
		visible: hasChildren
	}

	CheckBox
	{
		name:		"alwaysDisplayModelInformation"
		text:		qsTr("Always display model information")
		checked:	false
		info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Always display model information (distribution, number of components, and model name) in output tables.") : qsTr("Always display model information (distribution and model name) in output tables.")
	}
}
