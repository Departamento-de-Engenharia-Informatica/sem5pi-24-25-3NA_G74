import { Observable } from 'rxjs';
import { Patient } from '../models/patient.model';

export interface IPatientRepository {
  createPatientProfile(patient: Patient): Observable<Patient>;

  updatePatientProfile(patient: Patient, medicalRecordNumber : string): Observable<Patient>;
  
  markPatientProfileAsDeleted(medicalRecordNumber: string): Observable<any>;
  
}