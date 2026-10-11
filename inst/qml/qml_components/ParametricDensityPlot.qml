import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

CheckBox
{
	id: densityRoot
	property bool rightCensoring: false
	property bool mergeDistributionsAvailable: false
	property var analysisForm
	property var modelPredictors: []
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false

	name: "densityPlot"
	label: qsTr("Density plot")
	checked: false
	info: qsTr("Compare the fitted distribution of survival times with the observed data.")

	ParametricObservedStrata
	{
		prefix: "densityPlot"
		analysisForm: densityRoot.analysisForm
		modelPredictors: densityRoot.modelPredictors
		covariateNames: densityRoot.covariateNames
		factorNames: densityRoot.factorNames
		predictorSourcesReady: densityRoot.predictorSourcesReady
	}

	CheckBox
	{
		name: "densityPlotObservedData"
		label: qsTr("Observed data")
		checked: true
		enabled: densityRoot.rightCensoring
		info: qsTr("Add a histogram of the observed survival times to the fitted density plot.")

		DropDown
		{
			name: "densityPlotObservedDataBinWidthType"
			id: densityPlotHistogramBinWidthType
			label: qsTr("Bin width type")
			startValue: "fd"
			info: qsTr("Choose the histogram bin width rule. Select Manual to specify the number of bins.")
			values:
			[
				{ label: qsTr("Sturges"), value: "sturges" },
				{ label: qsTr("Scott"), value: "scott" },
				{ label: qsTr("Doane"), value: "doane" },
				{ label: qsTr("Freedman-Diaconis"), value: "fd" },
				{ label: qsTr("Manual"), value: "manual" }
			]
		}

		IntegerField
		{
			name: "densityPlotObservedDataNumberOfBins"
			label: qsTr("Number of bins")
			defaultValue: 30
			min: 3
			max: 10000
			enabled: densityPlotHistogramBinWidthType.currentValue === "manual"
			info: qsTr("Set the number of histogram bins.")
		}
	}

	Group
	{
		title: qsTr("Merge Plots Across")
		columns: 2

		CheckBox
		{
			name: "densityPlotMergePlotsAcrossDistributions"
			label: qsTr("Distributions")
			checked: false
			enabled: densityRoot.mergeDistributionsAvailable
			info: qsTr("Overlay fitted density curves for the selected distributions.")
		}

	}

	CheckBox
	{
		name: "densityPlotConfidenceIntervals"
		label: qsTr("Confidence intervals")
		checked: false
		childrenOnSameRow: true
		info: qsTr("Show confidence intervals around the fitted density curve.")

		CIField
		{
			name: "densityPlotConfidenceIntervalsLevel"
			defaultValue: 95
			info: qsTr("Set the confidence level for the fitted density.")
		}
	}	
	
	DropDown
	{
		name: "densityPlotXAxisTransformation"
		label: qsTr("X-axis transformation")
		startValue: "log"
		info: qsTr("Choose the transformation for the x-axis. Log shows the distribution of log survival times.")
		values:
		[
			{ label: qsTr("None"), value: "none" },
			{ label: qsTr("Log"), value: "log" }
		]
	}

}
