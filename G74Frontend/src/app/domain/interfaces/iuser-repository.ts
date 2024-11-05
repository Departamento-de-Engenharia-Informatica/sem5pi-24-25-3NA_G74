import { Observable } from 'rxjs';
import {User} from '../models/user.model';

export interface IUserRepository {
  registerUser(user: User): Observable<User>;
  updateUser(user: Partial<User>): Observable<User>;
  markUserAsDeleted(): Observable<any>;

}
