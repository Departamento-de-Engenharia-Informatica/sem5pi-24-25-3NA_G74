import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';
import { PatientRepository } from '../../infrastructure/repositories/patient-repository';


@Injectable({
    providedIn: 'root'
})
export class PatientService {
    constructor(private patientRepository: PatientRepository) { }

    createPatientProfile(patient: Patient): Observable<Patient> {
        return this.patientRepository.createPatientProfile(patient);
    }

    updatePatientProfile(patient: Partial<Patient>, medicalRecordNumber: string): Observable<Patient> {
        return this.patientRepository.updatePatientProfile(patient, medicalRecordNumber);
    }

    markPatientProfileAsDeleted(medicalRecordNumber: string): Observable<any> {
        return this.patientRepository.markPatientProfileAsDeleted(medicalRecordNumber);
    }

}

