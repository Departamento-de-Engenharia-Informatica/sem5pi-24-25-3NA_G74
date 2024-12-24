import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import { Observable, catchError, throwError, tap, map } from 'rxjs';
import { HttpParams } from '@angular/common/http';
import { MedicalConditionDto } from '../../dto/medicalCondition.dto';
import { MedicalConditionMapper } from '../../mappers/medicalConditionMapper';


@Injectable({
    providedIn: 'root'
})


export class MedicalConditionService {

    private apiUrl = `${environment.nodeApiUrl}/medical-conditions/`;

    constructor(private http: HttpClient) { }


    createMedicalCondition(medicalCondition: MedicalConditionDto): Observable<MedicalConditionDto> {

        return this.http.post<MedicalConditionDto>(this.apiUrl, medicalCondition).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );

    }

    updateMedicalCondition(medicalCondition: MedicalConditionDto): Observable<MedicalConditionDto> {
        // PATCH /medical-conditions
        return this.http.patch<MedicalConditionDto>(this.apiUrl, medicalCondition)
            .pipe(
                tap(response => console.log('Update response:', response)),
                catchError(error => {
                    console.error('Update error:', error);
                    return throwError(() => new Error('Failed to update medical condition.'));
                })
            );
    }

    searchMedicalCondition(medicalConditionCode?: string, designation?: string): Observable<MedicalConditionDto[]> {

        const params = this.buildQueryParams(medicalConditionCode, designation);

        return this.http.get<MedicalConditionDto[]>(this.apiUrl, { params }).pipe(
            map((dtos: MedicalConditionDto[]) => {
                return dtos.map((dto) => {
                    const domainModel = MedicalConditionMapper.toDomain(dto);
                    return MedicalConditionMapper.toDto(domainModel);
                });
            }),
            catchError((error) => {
                console.error('Error fetching medical conditions:', error);
                return throwError(() => new Error('Failed to fetch medical conditions.'));
            })
        );

    }

    private buildQueryParams(medicalConditionCode?: string, designation?: string): HttpParams {
        let params = new HttpParams();

        if (medicalConditionCode) {
            params = params.set('medicalConditionCode', medicalConditionCode);
        }
        if (designation) {
            params = params.set('designation', designation);
        }
        return params;
    }

}