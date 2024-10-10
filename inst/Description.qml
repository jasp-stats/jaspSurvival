import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name:			"jaspSurvival"
	title : 		qsTr("Survival (beta)")
	description:	qsTr("Perform analyses of censored time to event data.")
	requiresData:	true
	icon:			"survival-analysis.svg"
	version			: "0.19.0"
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
		requiresData:	true
	}

	Analysis
	{
		menu:			qsTr("Semi-parametric")
		title:			qsTr("Semi-parametric Survival Analysis")
		func:			"SemiParametricSurvivalAnalysis"
		requiresData:	true
	}
/*
  	Analysis
	{
		menu:			qsTr("Parametric")
		title:			qsTr("Parametric Survival Analysis")
		func:			"ParametricSurvivalAnalysis"
		requiresData:	true
	}
*/
}
