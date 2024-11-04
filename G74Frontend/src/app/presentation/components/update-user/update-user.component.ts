import { Component } from '@angular/core';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';
import {NgIf} from '@angular/common';
import {UserService} from '../../../application/services/user.service';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {User} from '../../../domain/models/user.model';

@Component({
  selector: 'app-update-user',
  templateUrl: './update-user.component.html',
  styleUrl: './update-user.component.css'
})
export class UpdateUserComponent {
  username: string = '';
  email: string = '';
  user: Partial<User> = {};
  message: string = '';

  constructor(private userService: UserService) {
  }

  onSubmit():void{

    let username;
    if (this.username) {
      username = this.username;
    }
    let email;
    if (this.email) {
      email = this.email;
    }

    this.userService.updateUser(this.user).pipe(
      catchError(error => {
        console.error('Error updating user profile:', error);
        this.message = `Failed to update user profile. ${error?.error?.message || 'Please try again.'}`;
        return of(null);
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'User profile updated successfully!';
        this.resetForm();
      }
    });
  }

  resetForm(): void{
    this.username = '';
    this.email = '';
  }

}
