import {Injectable} from '@angular/core';
import {environment} from '../../../environments/environment';
import {HttpClient} from '@angular/common/http';
import {OperationType} from '../models/operationType.model';
import { HttpParams } from '@angular/common/http';
import {Patient} from '../models/patient.model';
import {Observable} from 'rxjs';

@Injectable({
  providedIn: 'root'
})

export class OperationTypeService{

  private apiUrl = `${environment.apiUrl}/OperationType/type/`;

  constructor(private http: HttpClient) {}

  listOperationType(operationtype: Partial<OperationType> | null): Observable<OperationType[]> {

    const url = `${this.apiUrl}find`;

    const params = operationtype ? this.buildQueryParams(operationtype) : new HttpParams();
    console.log('Request URL:', url);
    console.log('Request Params:', params.toString());
    return this.http.get<OperationType[]>(url, { params });
  }



  private buildQueryParams(operationtype: Partial<OperationType>): HttpParams {
    let params = new HttpParams();

    const hasValidParams = Object.values(operationtype).some(
      value => value !== undefined && value !== null && value !== ''
    );

    if (!hasValidParams) {
      return params;
    }

    for (const key in operationtype) {
      const value = operationtype[key as keyof OperationType];
      if (value !== undefined && value !== null && value !== '') {
        params = params.set(key, value.toString());
      }
    }

    return params;
  }


}
