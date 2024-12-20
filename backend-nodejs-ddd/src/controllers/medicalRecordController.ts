import { Request, Response, NextFunction } from 'express';
import { BaseController } from '../core/infra/BaseController';
import IMedicalConditionController from './IControllers/IMedicalConditionController';
import IMedicalRecordController from './IControllers/IMedicalRecordController';
import { Inject, Service } from 'typedi';
import config from '../../config';
import IMedicalRecordService from '../services/IServices/IMedicalRecordService';

@Service()
export default class MedicalRecordController extends BaseController implements IMedicalRecordController {
  constructor(@Inject(config.services.medicalRecord.name) private medicalRecordServiceInstance: IMedicalRecordService) {
    super();
  }
  protected executeImpl(): Promise<void | any> {
    throw new Error('Method not implemented.');
  }
  updateMedicalCondition(req: Request, res: Response, next: NextFunction) {
    throw new Error('Method not implemented.');
  }
}
