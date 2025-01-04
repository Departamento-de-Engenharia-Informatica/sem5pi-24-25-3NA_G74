import { Injectable, inject } from '@angular/core';
import { SpecializationService } from '../services/specialization.repository';
import { Specialization } from '../../domain/models/specialization.model';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class SpecializationViewModel {
    specializationRepository = inject(SpecializationService);
  

  createSpecialization(specialization: Specialization): Observable<Specialization> {
    return this.specializationRepository.createSpecialization(specialization);
  }

  getAllSpecialization(): Observable<Specialization[]> {
    return this.specializationRepository.getAll();
  }

  getSpecializationByCode(code: string): Observable<Specialization> {
    return this.specializationRepository.getByCode(code);
  }
}