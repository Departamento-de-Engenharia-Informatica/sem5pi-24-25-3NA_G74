import { Observable } from 'rxjs';
import {Patient } from '../models/patient.model';

export interface IPatientRepository {
  getUsers(): Observable<Patient[]>;
  getUserById(id: string): Observable<Patient>;
}