import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';
import { IPatientRepository } from '../../domain/interfaces/ipatient-repository';
import { environment } from '../../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class PatientRepository implements IPatientRepository {
  private apiUrl = `${environment.apiUrl}/users`;

  constructor(private http: HttpClient) {}

  getUsers(): Observable<Patient[]> {
    return this.http.get<Patient[]>(this.apiUrl);
  }

  getUserById(id: string): Observable<Patient> {
    return this.http.get<Patient>(`${this.apiUrl}/${id}`);
  }
}