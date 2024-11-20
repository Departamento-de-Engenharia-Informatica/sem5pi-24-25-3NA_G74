import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import {User} from "../../domain/models/user.model";
import {UserRepository} from "../../infrastructure/repositories/user-repository";

@Injectable({
  providedIn: 'root'
})
export class UserService {
  constructor(private userRepository: UserRepository) {}

  registerUser(user: User): Observable<User> {
    return this.userRepository.registerUser(user);
  }

  updateUser(email: string,user: Partial<User>): Observable<User>{
    return this.userRepository.updateUser(email,user);
  }

  markUserAsDeleted(email: string): Observable<any>{
    return this.userRepository.markUserAsDeleted(email);
  }

}
