import QtQuick
import JASP.Module

Description
{
	name:			"jaspSurvival"
	title : 		qsTr("Survival")
	description:	qsTr("Perform analyses of censored time to event data.")
	requiresData:	true
	preloadData: 	true
	icon:			"survival-analysis.svg"
	version			: "0.20.0"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"survival-analysis.svg"
	}

	Analysis
	{
		menu:			qsTr("Non-parametric")
		title:			qsTr("Non-parametric Survival Analysis")
		func:			"NonParametricSurvivalAnalysis"
	}

	Analysis
	{
		menu:			qsTr("Semi-parametric")
		title:			qsTr("Semi-parametric Survival Analysis")
		func:			"SemiParametricSurvivalAnalysis"
	}

  	Analysis
	{
		menu:			qsTr("Parametric (beta)")
		title:			qsTr("Parametric Survival Analysis")
		func:			"ParametricSurvivalAnalysis"
	}

}
