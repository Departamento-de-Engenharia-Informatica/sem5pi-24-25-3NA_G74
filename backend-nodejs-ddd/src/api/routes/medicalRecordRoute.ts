import { Router } from "express";
import { celebrate, Joi } from "celebrate";

import { Container } from "typedi";


import config from "../../../config";
import common from "mocha/lib/interfaces/common";
import IMedicalRecordController from "../../controllers/IControllers/IMedicalRecordController";

const route = Router();

export default (app: Router) =>
{
    app.use("/medical-record", route);

    const ctrl = Container.get(config.controllers.medicalRecord.name) as IMedicalRecordController;

    route.patch(
        '',
        celebrate({
            body: Joi.object({
                allergies: Joi.array().items(Joi.string().guid({ version: 'uuidv4' })).optional(),
                medicalConditions: Joi.array().items(Joi.string().guid({ version: 'uuidv4' })).optional(),
            }).or('allergies', 'medicalConditions'),
        }),
        (req, res, next) => ctrl.updateMedicalCondition(req, res, next)
    );
}