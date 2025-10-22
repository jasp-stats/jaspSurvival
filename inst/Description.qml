import QtQuick
import JASP.Module

Description
{
	title : 		qsTr("Survival")
	description:	qsTr("Perform analyses of censored time to event data.")
	icon:			"survival-analysis.svg"
	hasWrappers: 	false
	
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
		menu:			qsTr("Parametric")
		title:			qsTr("Parametric Survival Analysis")
		func:			"ParametricSurvivalAnalysis"
	}

}
