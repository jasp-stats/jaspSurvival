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

	IntegerField
	{
		name:			"confidenceIntervalSimulationDraws"
		label:			qsTr("Confidence interval simulation draws")
		defaultValue:	10000
		min:			100
		max:			1000000
		info: qsTr("Set the number of parameter draws used to simulate confidence intervals for prediction tables and all model-based plot bands. More draws improve precision at a higher computational cost.")
	}

	SetSeed {}
}
