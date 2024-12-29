"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const celebrate_1 = require("celebrate");
const typedi_1 = require("typedi");
const medicalRecordService_1 = __importDefault(require("../../services/medicalRecordService"));
const medicalRecordController_1 = __importDefault(require("../../controllers/medicalRecordController"));
const route = (0, express_1.Router)();
exports.default = (app) => {
    app.use('/medical-record', route);
    // Get all medical records
    route.get('', // empty string means this handles the base route /medical-record
    async (req, res, next) => {
        try {
            const medicalRecordServiceInstance = typedi_1.Container.get(medicalRecordService_1.default);
            const { records } = await medicalRecordServiceInstance.getAll();
            return res.status(200).json({ records });
        }
        catch (e) {
            return next(e);
        }
    });
    route.get('/:patientId', (0, celebrate_1.celebrate)({
        params: celebrate_1.Joi.object({
            patientId: celebrate_1.Joi.string().required(),
        }),
    }), async (req, res, next) => {
        try {
            console.log(req.body);
            const medicalRecordServiceInstance = typedi_1.Container.get(medicalRecordService_1.default);
            const record = await medicalRecordServiceInstance.getByPatientId(req.params.patientId);
            return res.status(200).json({ record });
        }
        catch (e) {
            return next(e);
        }
    });
    route.post('', (0, celebrate_1.celebrate)({
        body: celebrate_1.Joi.object({
            patientId: celebrate_1.Joi.string().required(),
            allergies: celebrate_1.Joi.array().items(celebrate_1.Joi.string().required()).optional(),
            medicalConditions: celebrate_1.Joi.array().items(celebrate_1.Joi.string().required()).optional(),
            freeText: celebrate_1.Joi.string(),
        }),
    }), async (req, res, next) => {
        try {
            const medicalRecordServiceInstance = typedi_1.Container.get(medicalRecordService_1.default);
            const record = await medicalRecordServiceInstance.create(req.body);
            return res.status(201).json({ record });
        }
        catch (e) {
            return next(e);
        }
    });
    route.patch('/:patientId', (0, celebrate_1.celebrate)({
        params: celebrate_1.Joi.object({
            patientId: celebrate_1.Joi.string().required(),
        }),
        body: celebrate_1.Joi.object({
            freeText: celebrate_1.Joi.string().allow('', null),
            allergies: celebrate_1.Joi.array().items(celebrate_1.Joi.string().required()).optional(),
            medicalConditions: celebrate_1.Joi.array().items(celebrate_1.Joi.string().required()).optional(),
        }),
    }), async (req, res, next) => {
        try {
            const medicalRecordControllerInstance = typedi_1.Container.get(medicalRecordController_1.default);
            const record = await medicalRecordControllerInstance.updateByPatientId(req.params.patientId, req.body);
            return res.status(200).json({ record });
        }
        catch (e) {
            return next(e);
        }
    });
    route.get('/medicalCondition/:medicalConditionCode', (0, celebrate_1.celebrate)({
        params: celebrate_1.Joi.object({
            medicalConditionCode: celebrate_1.Joi.string().required(),
        })
    }), async (req, res, next) => {
        try {
            console.log('Received request for medical condition:', req.params.medicalConditionCode);
            const medicalRecordControllerInstance = typedi_1.Container.get(medicalRecordController_1.default);
            const records = await medicalRecordControllerInstance.findByMedicalCondition(req.params.medicalConditionCode);
            console.log('Sending response with records');
            return res.status(200).json({ records });
        }
        catch (e) {
            console.error('Error in route handler:', e);
            return next(e);
        }
    });
    route.get('/allergy/:allergyCode', (0, celebrate_1.celebrate)({
        params: celebrate_1.Joi.object({
            allergyCode: celebrate_1.Joi.string().required(),
        })
    }), async (req, res, next) => {
        try {
            console.log('Received request for allergy:', req.params.allergyCode);
            const medicalRecordControllerInstance = typedi_1.Container.get(medicalRecordController_1.default);
            const records = await medicalRecordControllerInstance.findByAllergy(req.params.allergyCode);
            console.log('Sending response with records');
            return res.status(200).json({ records });
        }
        catch (e) {
            console.error('Error in route handler:', e);
            return next(e);
        }
    });
};
//# sourceMappingURL=medicalRecordRoute.js.map