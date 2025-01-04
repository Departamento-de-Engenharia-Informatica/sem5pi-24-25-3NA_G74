import { Injectable, inject } from '@angular/core';
import { BehaviorSubject, Observable, throwError } from 'rxjs';
import { Staff } from '../models/staff.model';
import { environment } from '../../../environments/environment';
import { HttpClient, HttpParams } from '@angular/common/http';
import { catchError, map, tap } from 'rxjs/operators';
import { MedicalRecord } from '../models/medicalRecord.model';
import { MedicalRecordDTO } from '../../dto/medicalRecord.dto';

@Injectable({
    providedIn: 'root'
})
export class MedicalRecordService {
    http = inject(HttpClient);

    private apiUrl = `${environment.apiUrlNode}/medical-record`;


  
    constructor() {}

    getMedicalRecords(): Observable<{records: MedicalRecordDTO[] }> {
        return this.http.get<{ records: MedicalRecord[] }>(this.apiUrl).pipe(
            map(response => ({
                records: response.records.map(record => ({
                    medicalRecordCode: record.medicalRecordCode,
                    medicalConditions: record.medicalConditions,
                    allergies: record.allergies,
                    freeText: record.freeText
                }))
            }))
        );
    }

    searchByMedicalCondition(medicalCondition: string): Observable<{records: MedicalRecordDTO[]}>{
        return this.http.get<{ records: MedicalRecord[] }>(this.apiUrl + '/medicalCondition/' + medicalCondition).pipe(
            map(response => ({
                records: response.records.map(record => ({
                    medicalRecordCode: record.medicalRecordCode,
                    medicalConditions: record.medicalConditions,
                    allergies: record.allergies,
                    freeText: record.freeText
                }))
            }))
        );
    }
    
    searchByAllergies(allergy: string): Observable<{records: MedicalRecordDTO[]}>{
        return this.http.get<{ records: MedicalRecord[] }>(this.apiUrl + '/allergy/' + allergy).pipe(
            map(response => ({
                records: response.records.map(record => ({
                    medicalRecordCode: record.medicalRecordCode,
                    medicalConditions: record.medicalConditions,
                    allergies: record.allergies,
                    freeText: record.freeText
                }))
            }))
        );
    }

    searchByPatientId(id: string){
        return this.http.get<{ records: MedicalRecord[] }> (this.apiUrl + id);
    }

    edit(medicalRecordCode: string, medicalRecord: MedicalRecordDTO): Observable<MedicalRecordDTO> {
        if (!medicalRecordCode) {
            return throwError(() => new Error('Medical record code is required.'));
        }

        const url = `${this.apiUrl}/${medicalRecordCode}`;
        return this.http.patch<MedicalRecordDTO>(url, medicalRecord)
            .pipe(
                tap(response => console.log('Update response:', response)),
                catchError(error => {
                    console.error('Update error:', error);
                    return throwError(() => new Error('Failed to update medical record.'));
                })
            );
    }

    create(medicalRecordDTO: MedicalRecordDTO){
        return this.http.post<MedicalRecordDTO>(this.apiUrl, medicalRecordDTO).pipe(
            tap(response => console.log('Received response from backend:', response)), 
            catchError(error => {
                console.error('Error response from backend:', error); 
                throw error; 
            })
        );
    }
   
    // addMedicalRecord(medicalRecord: MedicalRecordDTO): Observable<MedicalRecord> {
    //     return this.http.post<MedicalRecordDTO>(this.apiUrl, medicalRecord).pipe(
    //         tap(response => console.log('Received response from backend:', response)), 
    //         catchError(error => {
    //             console.error('Error response from backend:', error); 
    //             throw error; 
    //         })
    //     );
    // }
    
    // updateMedicalRecord(medicalRecord: MedicalRecordDTO): Observable<MedicalRecord> {
    //     return this.http.patch<MedicalRecordDTO>(this.apiUrl, medicalRecord)
    //         .pipe(
    //             tap(response => console.log('Update response:', response)),
    //             catchError(error => {
    //                 console.error('Update error:', error);
    //                 return throwError(() => new Error('Failed to update medical record.'));
    //             })
    //         );
    // }

    // searchMedicalCondition(medicalConditionCode?: string, designation?: string): Observable<MedicalConditionDto[]> {

    //     const params = this.buildQueryParams(medicalConditionCode, designation);

    //     return this.http.get<MedicalConditionDto[]>(this.apiUrl, { params }).pipe(
    //         map((dtos: MedicalConditionDto[]) => {
    //             return dtos.map((dto) => {
    //                 const domainModel = MedicalConditionMapper.toDomain(dto);
    //                 return MedicalConditionMapper.toDto(domainModel);
    //             });
    //         }),
    //         catchError((error) => {
    //             console.error('Error fetching medical conditions:', error);
    //             return throwError(() => new Error('Failed to fetch medical conditions.'));
    //         })
    //     );

    // }


    // private buildQueryParams(patientId: string,medicalConditions?: string[], allergies?: string[], freeText: string): HttpParams {
    //     let params = new HttpParams();

    //     if (patientId) {
    //         params = params.set('patientId', patientId);
    //     }

    //     if (medicalConditions) {
    //         medicalConditions.forEach(condition => {
    //             params = params.append('medicalConditions', condition);
    //         });
    //     }

    //     if (allergies){
    //         allergies.forEach(allergies => {
    //             params = params.append('allergies', allergies);
    //         })
    //     }

    //     if(freeText){
    //         params = params.set('freeText', freeText);
    //     }

    //     return params;
    // }
    
    // deleteMedicalRecord(id: number): void {
        
    // }
    
    // private getNextId(): number {
    //     return 0;
    // }


}
