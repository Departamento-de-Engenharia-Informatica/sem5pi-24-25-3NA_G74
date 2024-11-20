import { Injectable } from '@angular/core';
import {HttpClient, HttpParams} from '@angular/common/http';
import { Observable } from 'rxjs';


import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';
import { OperationType } from '../../domain/models/operationType.model';
import { IOperationTypeRepository } from '../../domain/interfaces/ioperationType-repository';
import {User} from '../../domain/models/user.model';

@Injectable({
  providedIn: 'root'
})

export class OperationTypeRepository implements IOperationTypeRepository {

  private apiUrl = `${environment.apiUrl}/type`;

  constructor(private http: HttpClient) {}

  listOperationTypesByFilter(criteria: Partial<OperationType>): Observable<OperationType[]> {
    console.log('Sending operation type data to backend:', criteria);
    const myURl = `${this.apiUrl}/find`;
    return this.http
      .get<OperationType[]>(this.apiUrl, { params: this.buildHttpParams(criteria) })
      .pipe(
        tap((operationTypes) => {
          console.log('Fetched operation types:', operationTypes);
        }),
        catchError((error) => {
          console.error('Error fetching operation types:', error);
          throw error;
        })
      );
  }

  private buildHttpParams(criteria: Partial<OperationType>): HttpParams {
    let params = new HttpParams();

    for (const key of Object.keys(criteria) as (keyof OperationType)[]) {
      const value = criteria[key];
      if (value !== undefined && value !== null) {
        params = params.set(key, String(value));
      }
    }

    return params;
  }


}
