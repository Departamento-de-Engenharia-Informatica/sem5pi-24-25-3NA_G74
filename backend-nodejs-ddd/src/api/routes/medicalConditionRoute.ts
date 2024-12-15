import { Router } from "express";
import { celebrate, Joi } from "celebrate";

import { Container } from "typedi";
import IMedicalConditionController from "../../controllers/IControllers/IMedicalConditionController";

import config from "../../../config";
import common from "mocha/lib/interfaces/common";

const route = Router();

export default (app: Router) => {
    app.use("/medical-conditions", route);

    const ctrl = Container.get(config.controllers.medicalCondition.name) as IMedicalConditionController;

    //console.log("Controller loaded:", ctrl);

    route.post(
        '',
        celebrate({
            body: Joi.object({
                medicalConditionCode: Joi.string().required(),
                designation: Joi.string().required(),
                description: Joi.string().required(),
                commonSymptoms: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.createMedicalCondition(req, res, next)
    );

    route.patch(
        '',
        celebrate({
            body: Joi.object({
                designation: Joi.string().optional(),
                description: Joi.string().optional(),
            }).or('designation', 'description'),
        }),
        (req, res, next) => ctrl.updateMedicalCondition(req, res, next)
    );

    route.get('', (req, res, next) => ctrl.searchMedicalCondition(req, res, next));
}