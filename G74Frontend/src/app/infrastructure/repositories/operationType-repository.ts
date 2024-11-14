import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';


import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';
import { OperationType } from '../../domain/models/operationType.model';
import { IOperationTypeRepository } from '../../domain/interfaces/ioperationType-repository';

@Injectable({
  providedIn: 'root'
})

export class OperationTypeRepository implements IOperationTypeRepository {

  private apiUrl = `${environment.apiUrl}/OperationRequest/`;

  constructor(private http: HttpClient) {}
    

    listAllOperationType(): Observable<OperationType[]> {
      console.log("Listando todas as Operation Requests")
      return this.http.get<OperationType[]>(this.apiUrl).pipe(
        tap(response => console.log('Received response from backend:', response)), 
        catchError(error => {
          console.error('Error response from backend:', error); 
          throw error; 
        })
      );
    }
    

}