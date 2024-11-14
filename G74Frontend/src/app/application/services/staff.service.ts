import { Injectable, inject } from '@angular/core';
import { StaffRepository } from '../../infrastructure/repositories/staff-repository';
import { Staff } from '../../domain/models/staff.model';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class StaffService {
  staffRepository = inject(StaffRepository);

  createStaffProfile(staff: Staff): Observable<Staff> {
    return this.staffRepository.createStaffProfile(staff);
  }
}
