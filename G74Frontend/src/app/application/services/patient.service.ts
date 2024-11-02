import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { Patient } from '../../domain/models/patient.model';
import { PatientRepository } from '../../infrastructure/repositories/patient-repository';

@Injectable({
  providedIn: 'root'
})
export class PatientService {

  /*
  constructor(private userRepository: UserRepository) {}

  loadUsers(): Observable<User[]> {
    return this.userRepository.getUsers();
  }

  getUser(id: string): Observable<User> {
    return this.userRepository.getUserById(id);
  }

  */
}