import { Router } from "express";
import { celebrate, Joi } from "celebrate";


import config from "../../../config";
import IAllergyController from "../../controllers/IControllers/IAllergyController";
import isAuth from "../middlewares/isAuth";
import checkRole from "../middlewares/checkRole";
import {Container} from "typedi";

const route = Router();

export default (app: Router) => {
    app.use("/allergy", route);

    const ctrl = Container.get(config.controllers.allergy.name) as IAllergyController;

    const Roles = {
        Admin: "Admin",
        Doctor: "Doctor",
        Patient: "Patient",
        Nurse: "Nurse",
        Technician: "Technician",
    }
    
    route.post(
        '',
        isAuth,
        checkRole([Roles.Admin]),
        celebrate({
            body: Joi.object({
                code: Joi.string().required(),
                designation: Joi.string().required(),
                description: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.createAllergy(req, res, next)
    );

    route.patch(
        '',
        isAuth,
        checkRole([Roles.Admin]),
        celebrate({
            body: Joi.object({
                designation: Joi.string().required(),
                description: Joi.string().required(),
            }),
        }),
        (req, res, next) => ctrl.updateAllergy(req, res, next)
    );

    route.get('', isAuth, checkRole([Roles.Doctor]), (req, res, next) => ctrl.searchAllergy(req, res, next));
}