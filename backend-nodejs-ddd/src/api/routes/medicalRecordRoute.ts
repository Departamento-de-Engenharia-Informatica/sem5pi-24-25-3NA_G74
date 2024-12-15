import { Router } from "express";
import { celebrate, Joi } from "celebrate";

import { Container } from "typedi";
import IMedicalConditionController from "../../controllers/IControllers/IMedicalConditionController";

import config from "../../../config";
import common from "mocha/lib/interfaces/common";
import IMedicalRecordController from "../../controllers/IControllers/IMedicalRecordController";

const route = Router();

export default (app: Router) =>
{
    app.use("/medical-record", route);

    const ctrl = Container.get(config.controllers.medicalCondition.name) as IMedicalRecordController;

    // route.patch(
    //     '',
    //     celebrate({
    //         body: Joi.object({
    //             designation: Joi.string().optional(),
    //             description: Joi.string().optional(),
    //         }).or('designation', 'description'),
    //     }),
    //     (req, res, next) => ctrl.updateMedicalCondition(req, res, next)
    // );
}