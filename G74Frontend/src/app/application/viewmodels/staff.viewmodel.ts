import { Injectable, inject } from '@angular/core';
import { StaffService } from '../services/staff.repository';
import { Staff } from '../../domain/models/staff.model';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class StaffViewModel {
  staffRepository = inject(StaffService);

  createStaffProfile(staff: Staff): Observable<Staff> {
    return this.staffRepository.createStaffProfile(staff);
  }

  updateStaffProfile(licenceNumber: string, staff: Staff): Observable<Staff> {
    return this.staffRepository.updateStaffProfile(licenceNumber, staff);
  }

  getAllStaff(): Observable<Staff[]> {
    return this.staffRepository.getAll();
  }

  getStaffByLicenceNumber(licenceNumber: string): Observable<Staff> {
    return this.staffRepository.getByLicenceNumber(licenceNumber);
  }

  deactivateStaff(licenceNumber: string): Observable<any> {
    return this.staffRepository.deactivateStaff(licenceNumber);
  }
}
