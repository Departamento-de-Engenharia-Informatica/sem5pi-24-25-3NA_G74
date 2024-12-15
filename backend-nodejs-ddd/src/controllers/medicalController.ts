import { Request, Response, NextFunction } from "express";
import { BaseController } from "../core/infra/BaseController";
import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import IMedicalRecordController from "./IControllers/IMedicalRecordController";

export default class MedicalController extends BaseController implements IMedicalRecordController
{
    protected executeImpl(): Promise<void | any> {
        throw new Error("Method not implemented.");
    }
    updateMedicalCondition(req: Request, res: Response, next: NextFunction) {
        throw new Error("Method not implemented.");
    }
    
}