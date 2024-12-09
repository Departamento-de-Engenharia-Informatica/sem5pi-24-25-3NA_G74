import { Router } from "express";
import { celebrate, Joi } from "celebrate";

import { Container } from "typedi";
import config from "../../../config";
import IAllergyController from "../../controllers/IControllers/IAllergyController";

const route = Router();

export default (app: Router) => {
    app.use("/allergy", route);

    const ctrl = Container.get(config.controllers.allergy.name) as IAllergyController;

    route.post(
        '',
        celebrate({
            body: Joi.object({
                description: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.createAllergy(req, res, next)
    );

    route.put(
        '',
        celebrate({
            body: Joi.object({
                description: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.updateAllergy(req, res, next)
    );

    route.get('', (req, res, next) => ctrl.searchAllergy(req, res, next));
}