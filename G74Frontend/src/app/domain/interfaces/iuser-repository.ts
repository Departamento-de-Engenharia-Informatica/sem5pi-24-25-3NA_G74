import { Observable } from 'rxjs';
import {User} from '../models/user.model';

export interface IUserRepository {
  registerUser(user: User): Observable<User>;

}
