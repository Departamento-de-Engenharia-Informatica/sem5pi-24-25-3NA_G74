import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { OperationRequestDTO } from '../../domain/models/operationRequest.model';
import { OperationRequest } from '../../domain/models/operationRequest.model';
import { IOperationRepository } from '../../domain/interfaces/ioperationRequest-repository';
import { environment } from '../../../environments/environment';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})

export class OperationRequestRepository implements IOperationRepository {

  private apiUrl = `https://localhost:7036/api/OperationRequest/`;

  constructor(private http: HttpClient) {}

    listAllOperationRequests(): Observable<OperationRequestDTO[]> {
      console.log("Listando todas as Operation Requests")
      return this.http.get<OperationRequestDTO[]>(this.apiUrl).pipe(
        tap(response => console.log('Received response from backend:', response)), 
        catchError(error => {
          console.error('Error response from backend:', error); 
          throw error; 
        })
      );
    }
    
    async createOperationRequest(operation: OperationRequest): Promise<OperationRequest> {
      
    const operationJson = JSON.stringify(operation);
    console.log('Operation Request JSON:', operationJson);
      try {
        const response = await this.http.post<OperationRequest>(this.apiUrl, operation).toPromise();
        console.log('Received response from backend:', response);
        if (!response) {
          throw new Error('Response is undefined');
        }
        return response;
      } catch (error) {
        console.error('Error response from backend:', error);
        throw error;
      }
      
    }
    
    updateOperationRequest(operation: Partial<OperationRequest>, id: number): Observable<OperationRequest> {
      const url = `${this.apiUrl}${id}`;
      const operationJson = JSON.stringify(operation);
      console.log(url)
      console.log('Operation Request JSON:', operationJson);
      return this.http.put<OperationRequest>(url, operation);
    }

    deleteOperationRequest(id: number): Observable<any> {
      const url = `${this.apiUrl}${id}`;
      return this.http.delete<any>(url);
    }

}
