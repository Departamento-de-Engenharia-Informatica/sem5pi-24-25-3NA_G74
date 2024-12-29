"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const celebrate_1 = require("celebrate");
const config_1 = __importDefault(require("../../../config"));
const isAuth_1 = __importDefault(require("../middlewares/isAuth"));
const checkRole_1 = __importDefault(require("../middlewares/checkRole"));
const typedi_1 = require("typedi");
const route = (0, express_1.Router)();
exports.default = (app) => {
    app.use("/allergy", route);
    const ctrl = typedi_1.Container.get(config_1.default.controllers.allergy.name);
    const Roles = {
        Admin: "Admin",
        Doctor: "Doctor",
        Patient: "Patient",
        Nurse: "Nurse",
        Technician: "Technician",
    };
    route.post('', isAuth_1.default, (0, checkRole_1.default)([Roles.Admin]), (0, celebrate_1.celebrate)({
        body: celebrate_1.Joi.object({
            code: celebrate_1.Joi.string().required(),
            designation: celebrate_1.Joi.string().required(),
            description: celebrate_1.Joi.string().required(),
        }),
    }), (req, res, next) => ctrl.createAllergy(req, res, next));
    route.patch('/:code', isAuth_1.default, (0, checkRole_1.default)([Roles.Admin]), (0, celebrate_1.celebrate)({
        body: celebrate_1.Joi.object({
            designation: celebrate_1.Joi.string().required(),
            description: celebrate_1.Joi.string().required(),
        }).or('designation', 'description'),
    }), (req, res, next) => ctrl.updateAllergy(req, res, next));
    //route.get('', isAuth, checkRole([Roles.Admin, Roles.Doctor]), (req, res, next) => ctrl.searchAllergy(req, res, next));
    route.get('', (req, res, next) => ctrl.searchAllergy(req, res, next));
};
//# sourceMappingURL=allergyRoute.js.map