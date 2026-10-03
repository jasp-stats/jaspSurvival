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
		menu:			qsTr("Non-Parametric")
		title:			qsTr("Non-Parametric Survival Analysis")
		func:			"NonParametricSurvivalAnalysis"
	}

	Analysis
	{
		menu:			qsTr("Semi-Parametric")
		title:			qsTr("Semi-Parametric Survival Analysis")
		func:			"SemiParametricSurvivalAnalysis"
	}

  	Analysis
	{
		menu:			qsTr("Parametric")
		title:			qsTr("Parametric Survival Analysis")
		func:			"ParametricSurvivalAnalysis"
	}

	Analysis
	{
		menu:			qsTr("Parametric Mixture")
		title:			qsTr("Parametric Mixture Survival Analysis")
		func:			"ParametricMixtureSurvivalAnalysis"
	}

}
