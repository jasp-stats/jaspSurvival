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


import QtQuick 2.8
import JASP.Controls 1.0

CheckBox
{
	name:	"plot"
	label:	qsTr("Plot")
	info: qsTr("This option generates a customizable survival plot to visualize the estimated survival probabilities over time from your survival analysis. You can choose different transformations of the survival probabilities to display, such as Survival, Risk, Cumulative Hazard, or Complementary Log-Log functions, depending on your analytical needs. The plot allows you to include confidence intervals to assess the precision of the estimates and add a risk table to provide detailed information about the number of subjects at risk and events over time. You can further enhance the plot by adding quantile lines to highlight specific survival times (e.g., median survival time) and adjust the legend position, color palette, and overall theme to suit your preferences.")

	DropDown
	{
		name:		"plotType"
		label:		qsTr("Type")
		info: qsTr("Select the type of statistic to plot: Survival, Risk, Cumulative Hazard, or Complementary Log-Log.")
		values:
		[
			{ label: qsTr("Survival"),					value: "survival"},
			{ label: qsTr("Risk"),						value: "risk"},
			{ label: qsTr("Cumulative hazard"),			value: "cumulativeHazard"},
			{ label: qsTr("Complementary log-log"),		value: "complementaryLogLog"}
		]
	}

	CheckBox
	{
		name:		"plotCi"
		label:		qsTr("Confidence interval")
		checked:	true
		info: qsTr("Include confidence intervals in the survival plot to show the variability around the estimates.")
	}

	CheckBox
	{
		name:		"plotRiskTable"
		label:		qsTr("Risk table")
		checked:	false
		info: qsTr("Add a risk table below the plot to display additional information about the number of subjects at risk over time.")

		CheckBox
		{
			name:		"plotRiskTableNumberAtRisk"
			label:		qsTr("Number at risk")
			checked:	true
			info: qsTr("Display the number of subjects at risk at each time point in the risk table.")
		}

		CheckBox
		{
			name:		"plotRiskTableCumulativeNumberOfObservedEvents"
			label:		qsTr("Cum. number of observed events")
			checked:	true
			info: qsTr("Display the cumulative number of observed events (e.g., deaths) at each time point in the risk table.")
		}

		CheckBox
		{
			name:		"plotRiskTableCumulativeNumberOfCensoredObservations"
			label:		qsTr("Cum. number of censored obs.")
			info: qsTr("Display the cumulative number of censored observations at each time point in the risk table.")
		}

		CheckBox
		{
			name:		"plotRiskTableNumberOfEventsInTimeInterval"
			label:		qsTr("Number of events in time interval")
			info: qsTr("Display the number of events that occurred within each time interval in the risk table.")
		}

		CheckBox
		{
			name:		"plotRiskTableNumberOfCensoredObservationsInTimeInterval"
			label:		qsTr("Number of censored obs. in time interval")
			info: qsTr("Display the number of censored observations within each time interval in the risk table.")
		}

		CheckBox
		{
			name:		"plotRiskTableAsASingleLine"
			label:		qsTr("As a single line")
			info: qsTr("Combine all selected risk table statistics into a single line for a more compact display.")
		}
	}

	CheckBox
	{
		name:		"plotAddQuantile"
		label:		qsTr("Add quantile")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Add a vertical line on the plot at a specified quantile (e.g., median survival time).")
		
		DoubleField
		{
			name:			"plotAddQuantileValue"
			defaultValue:	0.5	
			min:			0
			max:			1
			info: qsTr("Specify the quantile value (between 0 and 1) at which to add the vertical line on the plot.")
		}
	}


	DropDown
	{
		name:		"plotLegend"
		enabled:	strata.count > 0
		label:		qsTr("Legend")
		info: qsTr("Choose the position of the legend on the plot: Bottom, Right, Left, Top, or None. Only available when Strata variables are specified.")
		values:
		[
			{ label: qsTr("Bottom"),	value: "bottom"},
			{ label: qsTr("Right"),		value: "right"},
			{ label: qsTr("Left"),		value: "left"},
			{ label: qsTr("Top"),		value: "top"},
			{ label: qsTr("None"),		value: "none"}
		]
	}

	ColorPalette
	{
		enabled:	strata.count > 0
		info: qsTr("Customize the color palette used in the plot. Only available when Strata variables are specified.")
	}

	DropDown
	{
		name:		"plotTheme"
		label:		qsTr("Theme")
		info: qsTr("Select the theme for the plot's appearance: JASP for the default look or ggsurvfit for the ggsurvfit package style.")
		values:
		[
			{ label: qsTr("JASP"),			value: "jasp"},
			{ label: qsTr("ggsurvfit"),		value: "ggsurvfit"}
		]
	}
}
