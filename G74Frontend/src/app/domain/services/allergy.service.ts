import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import { Observable, catchError, throwError, tap, map } from 'rxjs';
import { HttpParams } from '@angular/common/http';
import {AllergyDTO} from "../../dto/allergy.dto";
import {AllergyMapper} from "../../mappers/AllergyMapper";


@Injectable({
    providedIn: 'root'
})


export class AllergyService {

    private apiUrl = `${environment.apiUrlNode}/allergy/`;

    constructor(private http: HttpClient) { }


    createAllergy(allergy: AllergyDTO): Observable<AllergyDTO> {

        return this.http.post<AllergyDTO>(this.apiUrl, allergy).pipe(
            tap(response => console.log('Received response from backend:', response)), // Log successful response
            catchError(error => {
                console.error('Error response from backend:', error); // Log error response
                throw error; // Re-throw error to be handled by calling service
            })
        );

    }

    updateAllergy(allergy: AllergyDTO): Observable<AllergyDTO> {
        // PATCH /allergy
        return this.http.patch<AllergyDTO>(this.apiUrl, allergy)
            .pipe(
                tap(response => console.log('Update response:', response)),
                catchError(error => {
                    console.error('Update error:', error);
                    return throwError(() => new Error('Failed to update medical condition.'));
                })
            );
    }

    searchAllergy(code?: string): Observable<AllergyDTO[]> {

        const params = this.buildQueryParams(code);

        return this.http.get<AllergyDTO[]>(this.apiUrl, { params }).pipe(
            map((dtos: AllergyDTO[]) => {
                return dtos.map((dto) => {
                    const domainModel = AllergyMapper.toDomain(dto);
                    return AllergyMapper.toDto(domainModel);
                });
            }),
            catchError((error) => {
                console.error('Error fetching allergies:', error);
                return throwError(() => new Error('Failed to fetch allergies.'));
            })
        );

    }

    private buildQueryParams(code?: string): HttpParams {
        let params = new HttpParams();

        if (code) {
            params = params.set('code', code);
        }
        return params;
    }

}
