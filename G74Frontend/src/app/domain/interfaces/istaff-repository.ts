import { Observable } from 'rxjs';
import { Staff } from '../models/staff.model';

export interface IStaffRepository {
    createStaffProfile(staff: Staff): Observable<Staff>;
}
