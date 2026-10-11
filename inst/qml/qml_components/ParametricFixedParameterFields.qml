pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

RowLayout
{
	id: fields
	property string distribution
	spacing: 8 * jaspTheme.uiScale

	ParametricFixedParameter
	{
		name: "rate"
		label: qsTr("rate")
		visible: ["exponential", "gamma", "gompertz"].indexOf(fields.distribution) >= 0
	}
	ParametricFixedParameter
	{
		name: "shape"
		label: qsTr("shape")
		visible: ["gamma", "gompertz", "logLogistic", "weibull", "generalizedGammaOriginal"].indexOf(fields.distribution) >= 0
		positive: fields.distribution !== "gompertz"
	}
	ParametricFixedParameter
	{
		name: "scale"
		label: qsTr("scale")
		visible: ["logLogistic", "weibull", "generalizedGammaOriginal"].indexOf(fields.distribution) >= 0
	}
	ParametricFixedParameter
	{
		name: "meanlog"
		label: qsTr("meanlog")
		visible: fields.distribution === "logNormal"
		positive: false
	}
	ParametricFixedParameter
	{
		name: "sdlog"
		label: qsTr("sdlog")
		visible: fields.distribution === "logNormal"
	}
	ParametricFixedParameter
	{
		name: "mu"
		label: "mu"
		visible: ["generalizedF", "generalizedGamma", "generalizedFOriginal"].indexOf(fields.distribution) >= 0
		positive: false
	}
	ParametricFixedParameter
	{
		name: "sigma"
		label: "sigma"
		visible: ["generalizedF", "generalizedGamma", "generalizedFOriginal"].indexOf(fields.distribution) >= 0
	}
	ParametricFixedParameter
	{
		name: "Q"
		label: "Q"
		visible: ["generalizedF", "generalizedGamma"].indexOf(fields.distribution) >= 0
		positive: false
	}
	ParametricFixedParameter
	{
		name: "P"
		label: "P"
		visible: fields.distribution === "generalizedF"
	}
	ParametricFixedParameter
	{
		name: "k"
		label: "k"
		visible: fields.distribution === "generalizedGammaOriginal"
	}
	ParametricFixedParameter
	{
		name: "s1"
		label: "s1"
		visible: fields.distribution === "generalizedFOriginal"
	}
	ParametricFixedParameter
	{
		name: "s2"
		label: "s2"
		visible: fields.distribution === "generalizedFOriginal"
	}
}
