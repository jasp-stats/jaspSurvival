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

Section
{
	id: modelSection
	property var analysisForm
	property bool mixture: false
	property int componentCount: 1
	property var selector
	property bool locationUsesEyring: false
	property var rawPredictors: []
	property bool predictorsInitialized: false
	property var covariateNames: []
	property var factorNames: []
	property bool predictorSourcesReady: false
	readonly property int modelCount: modelTerms.count
	readonly property int variableCount: modelTerms.countVariables + (ancillary1.active ? ancillary1.variableCount : 0) + (ancillary2.active ? ancillary2.variableCount : 0) + (ancillary3.active ? ancillary3.variableCount : 0)
	readonly property var parameterSlots: ({
		exponential: [],
		gamma: ["shape"],
		generalizedF: ["sigma", "Q", "P"],
		generalizedGamma: ["sigma", "Q"],
		gompertz: ["shape"],
		logLogistic: ["shape"],
		logNormal: ["sdlog"],
		weibull: ["shape"],
		generalizedGammaOriginal: ["shape", "k"],
		generalizedFOriginal: ["sigma", "s1", "s2"]
	})
	readonly property var locationParameterByFamily: ({
		exponential: "rate",
		gamma: "rate",
		generalizedF: "mu",
		generalizedGamma: "mu",
		gompertz: "rate",
		logLogistic: "scale",
		logNormal: "meanlog",
		weibull: "scale",
		generalizedGammaOriginal: "scale",
		generalizedFOriginal: "mu"
	})
	readonly property var effectiveFamilies:
	{
		let selected = selector ? (["all", "bestAic", "bestBic"].indexOf(selector.value) >= 0 ? selector.selectedFamilies.map(function(item) { return item.value; }) : [selector.value]) : [];
		return locationUsesEyring ? selected.filter(function(family) { return family !== "gompertz"; }) : selected;
	}
	readonly property var locationParameters:
	{
		let parameters = [];
		for (let family of effectiveFamilies)
		{
			let parameter = locationParameterByFamily[family];
			if (parameters.indexOf(parameter) < 0) parameters.push(parameter);
		}
		return parameters;
	}
	function queueRefresh() { Qt.callLater(refreshModels); }
	function refreshModels()
	{
		if (!analysisForm || !analysisForm.initialized() || !modelTerms.initialized || !advancedSpecificationControl.initialized || (advancedSpecificationControl.checked && !locationModifiers.initialized)) return;
		let snapshot = analysisForm.options();
		// Convert only current named payloads: native QVariant sequences are not JS arrays.
		let locationModels = JSON.parse(JSON.stringify(snapshot[modelTerms.name]));
		let usesEyring = false;
		if (advancedSpecificationControl.checked)
		{
			let modifiers = JSON.parse(JSON.stringify(snapshot[locationModifiers.name]));
			let modelKeys = locationModels.map(function(model) { return model.name; });
			usesEyring = modifiers.some(function(model) {
				let modelKey = Array.isArray(model.model) ? model.model[0] : model.model;
				return modelKeys.indexOf(modelKey) >= 0 && Array.isArray(model.predictors) && model.predictors.some(function(predictor) {
					let variable = predictor.variable;
					if (variable && variable.value !== undefined) variable = variable.value;
					if (Array.isArray(variable)) variable = variable[0];
					if (variable && variable.value !== undefined) variable = variable.value;
					return modelSection.covariateNames.indexOf(variable) >= 0 && predictor.predictorTransformation === "eyring";
				});
			});
		}
		if (locationUsesEyring !== usesEyring) locationUsesEyring = usesEyring;
		let variables = [];
		let forms = [modelTerms];
		if (ancillary1.active) forms.push(ancillary1.termModels);
		if (ancillary2.active) forms.push(ancillary2.termModels);
		if (ancillary3.active) forms.push(ancillary3.termModels);
		for (let form of forms)
		{
			if (!form.initialized || form.count !== modelTerms.count)
			{
				if (predictorsInitialized) predictorsInitialized = false;
				return;
			}
			let models = form === modelTerms ? locationModels : JSON.parse(JSON.stringify(snapshot[form.name]));
			for (let model of models)
			{
				let terms = model.components && model.components.value;
				if (!Array.isArray(terms)) continue;
				for (let term of terms)
					for (let variable of (Array.isArray(term) ? term : [term]))
						if (variables.indexOf(variable) < 0) variables.push(variable);
			}
		}
		if (variables.length !== rawPredictors.length || variables.some(function(variable, index) { return variable !== rawPredictors[index]; }))
			rawPredictors = variables;
		if (!predictorsInitialized) predictorsInitialized = true;
	}
	onAnalysisFormChanged: queueRefresh()
	onCovariateNamesChanged: queueRefresh()
	property Connections analysisConnections: Connections
	{
		target: modelSection.analysisForm
		function onAnalysisChanged() { modelSection.queueRefresh(); }
	}
	function slotTitle(slot)
	{
		let parameters = [];
		for (let family of effectiveFamilies)
		{
			let parameter = parameterSlots[family][slot - 1];
			if (parameter && parameters.indexOf(parameter) < 0) parameters.push(parameter);
		}
		return parameters.length > 0 ? qsTr("Parameter model: %1").arg(parameters.join(", ")) : "";
	}
	readonly property bool selectionAllowsMerging: modelTerms.count == 1 || (modelTerms.count > 1 && interpretModel.value !== "bestAic" && interpretModel.value !== "bestBic")

	title: qsTr("Model")

	Group
	{
		columns: 1
		Layout.columnSpan: parent.columns
		preferredWidth: parent.width - jaspTheme.groupContentPadding
		title: ancillaryRegression.checked ? qsTr("Parameter model: %1").arg(modelSection.locationParameters.join(", ")) : ""
		FactorsForm
		{
			name:				"modelTerms"
			id:					modelTerms
			depends:			advancedSpecificationControl
			nested:				!advancedSpecificationControl.checked
			keepAvailableVariables:	advancedSpecificationControl.checked
			startIndex:			1
			initNumberFactors:	1
			allowInteraction:	true
			baseName:			"model"
			baseTitle:			qsTr("Model")
			availableVariablesListName:		"availableTerms"
			availableVariablesList.source:	['covariates', 'factors']
			allowedColumns:		[]
			onBoundValueChanged: modelSection.queueRefresh()
			onInitializedChanged: modelSection.queueRefresh()
		}
	}


	Group
	{
		columns: 1
		Layout.columnSpan: parent.columns
		preferredWidth: parent.width - jaspTheme.groupContentPadding

		CheckBox
		{
			id: advancedSpecificationControl
			onCheckedChanged: modelSection.queueRefresh()
			name: "modelAdvancedSpecification"
			label: qsTr("Advanced specification")
			enabled: modelTerms.countVariables > 0
			info: qsTr("Specify each model independently, and set coefficient restrictions, covariate transformations or factor contrasts.")
		}

		ParametricModelModifiers
		{
			id: locationModifiers
			name: "modelAdvancedSpecificationTerms"
			models: modelTerms
			mixture: modelSection.mixture
			componentCount: modelSection.componentCount
			covariateNames: modelSection.covariateNames
			factorNames: modelSection.factorNames
			predictorSourcesReady: modelSection.predictorSourcesReady
			visible: advancedSpecificationControl.enabled && advancedSpecificationControl.checked
			enabled: advancedSpecificationControl.enabled && advancedSpecificationControl.checked
			onBoundValueChanged: modelSection.queueRefresh()
			onInitializedChanged: modelSection.queueRefresh()
		}
	}
	CheckBox
	{
		id: ancillaryRegression
		name: "modelAncillaryParameterRegression"
		label: qsTr("Ancillary parameter regression")
		checked: false
		onCheckedChanged: modelSection.queueRefresh()
		info: qsTr("Use predictors to model additional distribution parameters, such as shape or scale.")
	}
	ParametricAncillaryEquation
	{
		id: ancillary1
		covariateNames: modelSection.covariateNames
		factorNames: modelSection.factorNames
		predictorSourcesReady: modelSection.predictorSourcesReady
		slot: 1
		locationModels: modelTerms
		mixture: modelSection.mixture
		componentCount: modelSection.componentCount
		equationTitle: modelSection.slotTitle(1)
		active: ancillaryRegression.checked && equationTitle.length > 0
		visible: active
		enabled: active
		onActiveChanged: modelSection.queueRefresh()
		onEquationTermsChanged: modelSection.queueRefresh()
	}
	ParametricAncillaryEquation
	{
		id: ancillary2
		covariateNames: modelSection.covariateNames
		factorNames: modelSection.factorNames
		predictorSourcesReady: modelSection.predictorSourcesReady
		slot: 2
		locationModels: modelTerms
		mixture: modelSection.mixture
		componentCount: modelSection.componentCount
		equationTitle: modelSection.slotTitle(2)
		active: ancillaryRegression.checked && equationTitle.length > 0
		visible: active
		enabled: active
		onActiveChanged: modelSection.queueRefresh()
		onEquationTermsChanged: modelSection.queueRefresh()
	}
	ParametricAncillaryEquation
	{
		id: ancillary3
		covariateNames: modelSection.covariateNames
		factorNames: modelSection.factorNames
		predictorSourcesReady: modelSection.predictorSourcesReady
		slot: 3
		locationModels: modelTerms
		mixture: modelSection.mixture
		componentCount: modelSection.componentCount
		equationTitle: modelSection.slotTitle(3)
		active: ancillaryRegression.checked && equationTitle.length > 0
		visible: active
		enabled: active
		onActiveChanged: modelSection.queueRefresh()
		onEquationTermsChanged: modelSection.queueRefresh()
	}
	DropDown
	{
		id:					interpretModel
		name:				"interpretModel"
		label:				qsTr("Interpret model")
		enabled:			modelTerms.count > 1
		onCountChanged:		if (!(value === "bestAic" || value === "bestBic" || value === "all")) currentIndex = count - 1
		info: mixture ? qsTranslate("ParametricMixtureSurvivalAnalysis", "Select a specified model, show all models, or select the best using AIC or BIC.") : qsTr("Select a specified model, show all models, or select the best using AIC or BIC.")
		startValue:			"model1"
		source:
		[
			{
				values: [
					{label: qsTr("All"),		value: "all"},
					{label: qsTr("Best AIC"),	value: "bestAic"},
					{label: qsTr("Best BIC"),	value: "bestBic"}
				]
			},
			{
				values: modelTerms.factorsTitles
			}
		]
	}

}
