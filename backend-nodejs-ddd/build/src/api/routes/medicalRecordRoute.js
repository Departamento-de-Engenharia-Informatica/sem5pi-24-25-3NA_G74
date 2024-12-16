"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const celebrate_1 = require("celebrate");
const typedi_1 = require("typedi");
const config_1 = __importDefault(require("../../../config"));
const route = (0, express_1.Router)();
exports.default = (app) => {
    app.use("/medical-record", route);
    const ctrl = typedi_1.Container.get(config_1.default.controllers.medicalRecord.name);
    route.patch('', (0, celebrate_1.celebrate)({
        body: celebrate_1.Joi.object({
            allergies: celebrate_1.Joi.array().items(celebrate_1.Joi.string().guid({ version: 'uuidv4' })).optional(),
            medicalConditions: celebrate_1.Joi.array().items(celebrate_1.Joi.string().guid({ version: 'uuidv4' })).optional(),
        }).or('allergies', 'medicalConditions'),
    }), (req, res, next) => ctrl.updateMedicalCondition(req, res, next));
};
//# sourceMappingURL=medicalRecordRoute.js.map