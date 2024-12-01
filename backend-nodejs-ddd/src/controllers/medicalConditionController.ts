import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import IMedicalConditionService from '../services/IServices/IMedicalConditionService';
import { IMedicalConditionDTO } from '../dto/IMedicalConditionDTO';


import { Result } from "../core/logic/Result";
import { BaseController } from '../core/infra/BaseController';


@Service()
export default class MedicalConditionController extends BaseController implements IMedicalConditionController {

    constructor(
        @Inject(config.services.medicalCondition.name) private medicalConditionServiceInstance: IMedicalConditionService
    ) {
        super();
    }

    protected executeImpl(): Promise<void | any> {
        throw new Error('Method not implemented.');
    }

    public async createMedicalCondition(req: Request, res: Response, next: NextFunction) {

        try {

            const medicalConditionOrError = await this.medicalConditionServiceInstance.createMedicalCondition(req.body as IMedicalConditionDTO) as Result<IMedicalConditionDTO>;

            if (medicalConditionOrError.isFailure) {
                return res.status(402).send();
            }

            const medicalConditionDTO = medicalConditionOrError.getValue();
            return this.created(res)

        } catch (e) {
            this.fail(e);
            return next(e);
        }
    }

    public async updateMedicalCondition(req: Request, res: Response, next: NextFunction) {
        try {
            const medicalConditionOrError = await this.medicalConditionServiceInstance.UpdateMedicalCondition(req.body as IMedicalConditionDTO) as Result<IMedicalConditionDTO>;

            if (medicalConditionOrError.isFailure) {
                return res.status(404).send();
            }

            const medicalConditionDTO = medicalConditionOrError.getValue();
            return this.ok(res, medicalConditionDTO);

        } catch (e) {
            this.fail(e);
            return next(e);
        }
    }

    public async searchMedicalCondition(req: Request, res: Response, next: NextFunction) {
        try {
            const medicalConditionOrError = await this.medicalConditionServiceInstance.SearchMedicalCondition(req.params.description) as Result<IMedicalConditionDTO[]>;

            if (medicalConditionOrError.isFailure) {
                return res.status(404).send();
            }

            const medicalConditionDTOs = medicalConditionOrError.getValue();
            return this.ok(res, medicalConditionDTOs);

        } catch (e) {
            this.fail(e);
            return next(e);
        }
    }

}