import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";
import {BaseController} from "../core/infra/BaseController";
import IAllergyService from "../services/IServices/IAllergyService";
import {IAllergyDTO} from "../dto/IAllergyDTO";
import {Result} from "../core/logic/Result";
import IAllergyController from "./IControllers/IAllergyController";

@Service()
export default class AllergyController extends BaseController implements IAllergyController {

    constructor(
        @Inject(config.services.allergy.name) private allergyServiceInstance: IAllergyService
    ) {
        super();
    }

    protected executeImpl(): Promise<void | any> {
        throw new Error('Method not implemented.');
    }

    public async createAllergy(req: Request, res: Response, next: NextFunction) {

        try {
            const allergyOrError = await this.allergyServiceInstance.CreateAllergy(req.body as IAllergyDTO) as Result<IAllergyDTO>;
            if (allergyOrError.isFailure) {
                return res.status(402).send();
            }

            const allergyDTO = allergyOrError.getValue();
            return this.created(res);

        } catch (e) {
            this.fail(e);
            return next(e);
        }
    }

    public async updateAllergy(req: Request, res: Response, next: NextFunction) {
        try {
            const allergyOrError = await this.allergyServiceInstance.UpdateAllergy(req.body as IAllergyDTO) as Result<IAllergyDTO>;

            if (allergyOrError.isFailure) {
                return res.status(404).send();
            }

            const allergyDTO = allergyOrError.getValue();
            return this.ok(res, allergyDTO);

        } catch (e) {
            this.fail(e);
            return next(e);
        }
    }

    public async searchAllergy(req: Request, res: Response, next: NextFunction) {
        try {
            const allergyOrError = await this.allergyServiceInstance.SearchAllergy(req.params.code) as Result<IAllergyDTO[]>;

            if (allergyOrError.isFailure) {
                return res.status(404).send();
            }

            const allergyDTOs = allergyOrError.getValue();
            return this.ok(res, allergyDTOs);

        } catch (e) {
            this.fail(e);
            return next(e);
        }
    }

}