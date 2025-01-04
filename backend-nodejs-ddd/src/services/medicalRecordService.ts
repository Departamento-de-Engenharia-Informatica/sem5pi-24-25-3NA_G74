import { Service, Inject } from 'typedi';
import MedicalRecordRepo from '../repos/medicalRecordRepo';
import IMedicalRecordService from './IServices/IMedicalRecordService';

@Service()
export default class MedicalRecordService implements IMedicalRecordService {
  
  constructor(@Inject('MedicalRecordRepo') private medicalRecordRepo: MedicalRecordRepo) {}

  public async getAll() {
    try {
      const records = await this.medicalRecordRepo.findAll();
      return { records };
    } catch (e) {
      throw e;
    }
  }

  public async getByPatientId(patientId: string) {
    try {
      const record = await this.medicalRecordRepo.findByPatientId(patientId);
      if (!record) {
        throw new Error('Medical record not found for this patient');
      }
      return { record };
    } catch (e) {
      throw e;
    }
  }

  public async create(recordData) {
    try {
      const record = await this.medicalRecordRepo.create(recordData);
      return record;
    } catch (e) {
      throw e;
    }
  }

  public async updateByPatientId(medicalRecordCode: string, updateData: any) {
    try {
      // Validate that record exists before update
      const existingRecord = await this.medicalRecordRepo.findByPatientId(medicalRecordCode);
      if (!existingRecord) {
        throw new Error('Medical record not found for this patient');
      }

      const record = await this.medicalRecordRepo.updateByPatientId(medicalRecordCode, updateData);
      return record;
    } catch (e) {
      throw e;
    }
  }

  public async findByMedicalCondition(medicalCondition: string) {
    try {
      console.log('Fetching all records');
      const records = await this.medicalRecordRepo.findAll();
      
      const filteredRecords = records.filter(record => 
        record.medicalConditions.some(condition => 
          condition.toLowerCase() === medicalCondition.toLowerCase()
        )
      );

      if (filteredRecords.length === 0) {
        throw new Error("Medical Condition doesn't exist.");
      }
      
      console.log('Filtered records:', filteredRecords);
      return filteredRecords;
    } catch (e) {
      console.error('Error in findByMedicalCondition:', e);
      throw e;
    }
  }
  
  public async findByAllergy(allergy2: string) {
    try {
      console.log('Fetching all records');
      const records = await this.medicalRecordRepo.findAll();
      
        const filteredRecords = records.filter(record => 
        record.allergies.some(allergy => 
          allergy.toLowerCase() === allergy2.toLowerCase()
        )
      );      
      if (filteredRecords.length === 0) {
        throw new Error("Allergy doesn't exist.");
      }
      
      console.log('Filtered records:', filteredRecords);
      return filteredRecords;
    } catch (e) {
      console.error('Error in findByMedicalCondition:', e);
      throw e;
    }
  }

  
}
