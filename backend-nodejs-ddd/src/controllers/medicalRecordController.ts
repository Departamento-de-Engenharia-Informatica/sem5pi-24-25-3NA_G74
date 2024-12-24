import { Request, Response, NextFunction } from 'express';
import { BaseController } from '../core/infra/BaseController';
import IMedicalConditionController from './IControllers/IMedicalConditionController';
import IMedicalRecordController from './IControllers/IMedicalRecordController';
import { Inject, Service } from 'typedi';
import config from '../../config';
import IMedicalRecordService from '../services/IServices/IMedicalRecordService';
import { MedicalRecord } from '../domain/medicalRecord';

@Service()
export default class MedicalRecordController extends BaseController implements IMedicalRecordController {

  constructor(@Inject(config.services.medicalRecord.name) private medicalRecordServiceInstance: IMedicalRecordService) {
    super();
  }

  protected executeImpl(): Promise<void | any> {
    throw new Error('Method not implemented.');
  }

  findByAllergy(allergy: string): Promise<MedicalRecord[]> {
    try{

      const medicalRecord = this.medicalRecordServiceInstance.findByAllergy(allergy);
      if(medicalRecord){return medicalRecord;}
      else{return null;}

    }catch(e){
      throw new Error("Stopped on Controller");
    }
  }
  
  findByMedicalCondition(medicalCondition: string): Promise<MedicalRecord[]> {
    try{

      const medicalRecords = this.medicalRecordServiceInstance.findByMedicalCondition(medicalCondition);
      if(medicalRecords){return medicalRecords}
      else{return null}

    }catch(e){
      throw new Error('Stopped in Controller.');
    }
  }

  updateByPatientId(patientId: string, updateData: any): Promise<MedicalRecord> {
    try{

      const medicalRecord = this.medicalRecordServiceInstance.updateByPatientId(patientId, updateData);
      if(medicalRecord){return medicalRecord}
      else{return null}

    }catch(e){

    }
    throw new Error('Method not implemented.');
  }
  
}
