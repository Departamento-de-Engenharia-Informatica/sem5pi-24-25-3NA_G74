import { Service, Inject } from 'typedi';
import MedicalRecordRepo from '../repos/medicalRecordRepo';

@Service()
export default class MedicalRecordService {
  constructor(@Inject('MedicalRecordRepo') private medicalRecordRepo: MedicalRecordRepo) {}

  public async getAll() {
    try {
      const records = await this.medicalRecordRepo.findAll();
      return { records };
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

  public async updateByPatientId(patientId: string, updateData: any) {
    try {
      // Validate that record exists before update
      const existingRecord = await this.medicalRecordRepo.findByPatientId(patientId);
      if (!existingRecord) {
        throw new Error('Medical record not found for this patient');
      }

      const record = await this.medicalRecordRepo.updateByPatientId(patientId, updateData);
      return record;
    } catch (e) {
      throw e;
    }
  }
}
