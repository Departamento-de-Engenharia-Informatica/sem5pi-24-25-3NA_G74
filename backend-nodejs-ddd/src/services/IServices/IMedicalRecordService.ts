import { Result } from '../../core/logic/Result';
import { MedicalRecord } from '../../domain/medicalRecord';
import IMedicalRecordDTO from '../../dto/IMedicalRecordDTO';


export default interface IMedicalRecordService {

  updateByPatientId(patientId: string, updateData: any): Promise<any>;

  findByMedicalCondition(medicalCondition: string): Promise<any[]>;
  
  findByAllergy(allergy: string): Promise<any[]>;

  getAll(): Promise<Result<IMedicalRecordDTO[]>>;

  getByPatientId(patientId: string): Promise<Result<IMedicalRecordDTO>>;


}
