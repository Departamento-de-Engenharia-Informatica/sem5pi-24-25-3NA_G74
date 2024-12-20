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
}
