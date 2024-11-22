import {Injectable} from '@angular/core';
import {Observable} from 'rxjs';
import {User} from '../../domain/models/user.model';
import {UserService} from '../../domain/services/user.service';

@Injectable({
  providedIn: 'root'
})

export class UserViewmodel{

  constructor(private userService: UserService) {
  }

  registerUser(user: User): Observable<User> {
    return this.userService.registerUser(user);
  }

  updateUser(email: string,user: Partial<User>): Observable<User>{
    return this.userService.updateUser(email,user);
  }

  markUserAsDeleted(email: string): Observable<any>{
    return this.userService.markUserAsDeleted(email);
  }

}
