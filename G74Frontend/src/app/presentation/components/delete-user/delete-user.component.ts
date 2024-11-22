import { Component } from '@angular/core';
import {catchError, of} from 'rxjs';
import {UserViewmodel} from '../../../application/viewmodels/user.viewmodel';

@Component({
  selector: 'app-delete-user',
  templateUrl: './delete-user.component.html',
  styleUrl: './delete-user.component.css'
})
export class DeleteUserComponent {
  message: string = '';
  email: string = '';

  constructor(private userViewmodel: UserViewmodel) {
  }

  onSubmit(): void {

    this.userViewmodel.markUserAsDeleted(this.email).pipe(
      catchError(error => {
        console.error('Error deleting user profile: ', error);
        this.message = `Failed to delete user profile. ${error?.error?.message || 'Please try again.'}`
        return of(null)
      })
    ).subscribe(response => {
      if (response) {
        this.message = 'User profile deleted successfully';
      }
    });
  }

}
