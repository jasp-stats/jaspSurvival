pragma Translator: "ParametricSurvivalAnalysis"

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	id: equation
	property int slot: 1
	property var locationModels
	property bool mixture: false
	property int componentCount: 1
	property string equationTitle: ""
	property bool active: false
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false
	signal equationTermsChanged()
	readonly property string prefix: "modelAncillaryParameter" + slot
	readonly property string modelPrefix: "ancillary" + slot
	readonly property int variableCount: terms.countVariables
	readonly property alias termModels: terms
	columns: 1
	Layout.columnSpan: parent.columns
	preferredWidth: parent.width - jaspTheme.groupContentPadding
	title: equationTitle
	ParametricAncillaryForm
	{
		id: terms
		name: equation.prefix + "ModelTerms"
		locationModels: equation.locationModels
		nested: false
		keepAvailableVariables: true
		baseName: equation.modelPrefix + "model"
		availableVariablesListName: equation.prefix + "AvailableTerms"
		availableVariablesList.source: ["covariates", "factors"]
		onBoundValueChanged: equation.equationTermsChanged()
		onInitializedChanged: equation.equationTermsChanged()
	}
	CheckBox
	{
		id: advanced
		name: equation.prefix + "AdvancedSpecification"
		label: qsTr("Advanced specification")
		enabled: terms.countVariables > 0
		info: qsTr("Set coefficient restrictions, transformations and factor contrasts for this parameter's models.")
	}
	ParametricModelModifiers
	{
		name: equation.prefix + "AdvancedSpecificationTerms"
		models: terms
		mixture: equation.mixture
		componentCount: equation.componentCount
		allowEyring: false
		covariateNames: equation.covariateNames
		factorNames: equation.factorNames
		predictorSourcesReady: equation.predictorSourcesReady
		visible: advanced.enabled && advanced.checked
		enabled: advanced.enabled && advanced.checked
	}
}
