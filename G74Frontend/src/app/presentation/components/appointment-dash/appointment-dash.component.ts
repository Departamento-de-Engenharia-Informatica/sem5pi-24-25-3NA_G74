import { HttpClient } from '@angular/common/http';
import { Component } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { ConfirmDialogComponent } from '../confirm-dialog/confirm-dialog.component';
import { catchError, of } from 'rxjs';
import { AppointmentDTO } from '../../../dto/appointment.dto';
import { AppointmentViewModel } from '../../../application/viewmodels/appointment.viewmodel';

@Component({
  selector: 'app-appointment-dash',
  templateUrl: './appointment-dash.component.html',
  styleUrls: ['./appointment-dash.component.css']
})
export class AppointmentDashComponent {
  appointment: AppointmentDTO = {
    operationRequestId: 0,
    operationTypeId: '',
    surgeryRoomId: 0,
    date: '',
    time: 0,
    status: 'Scheduled'
  };
  isLoading = false;
  message = '';

  constructor(private http: HttpClient, private dialog: MatDialog, private appointmentViewModel: AppointmentViewModel) {}

  onSubmit() {
    this.message = '';
    console.log(this.appointment);

    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      data: { message: 'Are you sure you want to add this new appointment?' }
    });

    dialogRef.afterClosed().subscribe(result => {
      if (result) {
        this.isLoading = true;

        if (!this.appointment.date) {
          console.error('Date is required.');
          alert('Date is required.');
          this.isLoading = false;
          return;
        }

        try {
          this.appointment.date = new Date(this.appointment.date).toISOString();
        } catch (error) {
          console.error('Invalid date format:', error);
          alert('Invalid date format. Please enter a valid date.');
          this.isLoading = false;
          return;
        }

        const apiUrl = 'https://localhost:5001/api/Appointment'; // Substitua pelo URL da sua API
        this.appointmentViewModel.create(this.appointment)
          .pipe(
            catchError(error => {
              console.error('Error creating appointment:', error);
              this.isLoading = false;
              return of(null);
            })
          )
          .subscribe((response: any) => {
            this.isLoading = false;
            if (response) {
              this.message = 'Appointment created successfully!';
              alert(this.message);
              this.resetForm();
              location.reload();
            }
          });
      } else {
        this.isLoading = false;
      }
    });
  }

  resetForm() {
    this.appointment = {
      operationRequestId: 0,
      operationTypeId: '',
      surgeryRoomId: 0,
      date: '',
      time: 0,
      status: 'Scheduled'
    };
  }
}