import { Router } from "express";
import { celebrate, Joi } from "celebrate";

import { Container } from "typedi";
import IMedicalConditionController from "../../controllers/IControllers/IMedicalConditionController";

import config from "../../../config";

const route = Router();

export default (app: Router) => {
    app.use("/medical-conditions", route);

    const ctrl = Container.get(config.controllers.medicalCondition.name) as IMedicalConditionController;

    route.post(
        '',
        celebrate({
            body: Joi.object({
                description: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.createMedicalCondition(req, res, next)
    );

    route.put(
        '',
        celebrate({
            body: Joi.object({
                description: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.updateMedicalCondition(req, res, next)
    );

    route.get('', (req, res, next) => ctrl.searchMedicalCondition(req, res, next));
}