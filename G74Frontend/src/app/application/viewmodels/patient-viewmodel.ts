import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';
import { PatientService } from '../../domain/services/patient.service';

@Injectable({
    providedIn: 'root'
})

export class PatientViewModel {

    constructor(private patientService: PatientService) { }

    createPatientProfile(patient: Patient): Observable<Patient> {
        return this.patientService.createPatientProfile(patient);
    }

    updatePatientProfile(patient: Partial<Patient>, medicalRecordNumber: string): Observable<Patient> {
        return this.patientService.updatePatientProfile(patient, medicalRecordNumber);
    }

    markPatientProfileAsDeleted(medicalRecordNumber: string): Observable<any> {
        return this.patientService.markPatientProfileAsDeleted(medicalRecordNumber);
    }

    listPatients(patient: Partial<Patient> | null): Observable<Patient[]> {
        return this.patientService.listPatients(patient);
    }

    getMedicalRecordNumber(email: string): Observable<string> {
        return this.patientService.getMedicalRecordNumber(email);
    }

}

