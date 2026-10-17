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
import "./qml_components" as SA

Form
{
	id: form
	info: qsTr("This analysis performs a parametric survival analysis.")

	property bool	categoricalPredictionLevelsPossible:		variables.factorCount > 0 && models.variableCount > 0
	property bool	mergeDistributionsAvailable:				distribution.value === "all" && models.selectionAllowsMerging

	SA.ParametricVariables
	{
		id: variables
		rightCensoring: censoring.rightCensoring
		intervalCensoring: censoring.intervalCensoring
		countingCensoring: censoring.countingCensoring
	}

	SA.ParametricCensoring
	{
		id: censoring
	}

	SA.ParametricDistribution
	{
		id: distribution
	}

	SA.ParametricModel
	{
		id: models
	}

	SA.ParametricStatistics
	{
		multipleModels:	models.modelCount > 1
		multipleResults:	distribution.value === "all" || models.modelCount > 1
	}

	SA.ParametricPredictions
	{
		rightCensoring:	censoring.rightCensoring
		mergeDistributionsAvailable:	form.mergeDistributionsAvailable
		categoricalLevelsPossible:	categoricalPredictionLevelsPossible
		subgroupsAvailable:	variables.subgroupCount > 0
	}

	Section
	{
		title:	qsTr("Diagnostics")

		SA.ParametricResidualPlots
		{
			rightCensoring:	censoring.rightCensoring
		}

		SA.ParametricProbabilityPlot
		{
			rightCensoring:	censoring.rightCensoring
			mergeDistributionsAvailable:	form.mergeDistributionsAvailable
			categoricalLevelsPossible:	categoricalPredictionLevelsPossible
			subgroupsAvailable:	variables.subgroupCount > 0
		}
	}

	SA.SurvivalExport
	{
		intervalCensoring: censoring.intervalCensoring
	}

	Section
	{
		title: qsTr("Advanced")

		SA.ParametricDistributions
		{
			selectedDistribution: distribution.value
		}

		SA.ParametricOutputFormatting
		{
			hasSubgroup: variables.subgroupCount > 0
			multipleModels: models.modelCount > 1
			selectedDistribution: distribution.value
		}

		SA.ParametricSimulation {}
	}
}
