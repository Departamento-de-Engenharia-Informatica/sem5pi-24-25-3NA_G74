import { Repo } from '../../core/infra/Repo';
import { MedicalRecord } from '../../domain/medicalRecord';

export default interface IMedicalRecordRepo extends Repo<MedicalRecord> {
  findAll(): Promise<MedicalRecord[]>;
}
